#!/usr/bin/perl
# dpp version 0.2.1
# Copyright (C) 1997 Dan Schmidt, <dfan@alum.mit.edu>,
#   see dpp.nw for full notice
# Don't modify this file, modify dpp.nw!
while ($option = shift) {
  if ($option =~ /^-N(.*)$/) { $plain{$1} = 1; }
  $compress_whitespace = 1, next if ($option =~ /^-cw/);
  $tex = 1, next if ($option =~ /^-tex/);
}
$TMPFILE = "dpp.tmp";
if (! -f STDIN) {
  open TMPFILE, "> $TMPFILE"
    or die "Couldn't open `$TMPFILE' for writing: $!; aborting";
  print TMPFILE while <STDIN>;
  close TMPFILE;
  open STDIN, "< $TMPFILE"
    or die "Couldn't open `$TMPFILE' for reading: $!; aborting";
  unlink $TMPFILE;
}
#pad:
#TRUE FALSE bool break case catch char class const
#                continue default delete do double else enum exit
#                extern false float for friend goto if inline int long
#                namespace new NULL operator private protected public
#                register return short signed sizeof static struct
#                switch template throw try true typedef union unsigned
#                vector virtual void whil

@keywords = qw( );
$incode = 0;
$delay = 1;
my ($texdefs);
$texdefs .=
  "\@literal \\def\\LA{\\begingroup\\maybehbox\\bgroup\\setupmodname\\Rm\$\\langle\$}\n" .
  "\@literal \\def\\RA{\$\\rangle\$\\egroup\\endgroup}\n";
$bm = "\\begin{math}";  $em = "\\end{math}";
$texdefs .= "\@literal \\newcommand{\\MM}{\\kern.5pt\\raisebox{.4ex}" .
            "{$bm\\scriptscriptstyle-\\kern-1pt-$em}\\kern.5pt}\n" .
            "\@literal \\newcommand{\\PP}{\\kern.5pt\\raisebox{.4ex}" .
            "{$bm\\scriptscriptstyle+\\kern-1pt+$em}\\kern.5pt}\n";
$texdefs .=
  "\@literal \\def\\commopen{/$bm\\ast\\,$em}\n" .
  "\@literal \\def\\commclose{\\,$bm\\ast$em\\kern-.5pt/}\n";
$texdefs .=
  "\@literal \\def\\begcomm{\\begingroup\\maybehbox\\bgroup\\setupmodname}\n" .
  "\@literal \\def\\endcomm{\\egroup\\endgroup}\n";

$text = $extra = "";
init_punc();
if (defined $tex) {
  $begcomm = "\\begcomm{}"; $endcomm = "\\endcomm{}";
}
while (<>) {
  if (/^\@text %keyword (.*)/) {
    push @keywords, (split " ", $1); next;
  }
  $in_quote = 1 if (/^\@quote/);
  $in_quote = 0 if (/^\@endquote/);
  if (! $in_quote) {
    if (/^\@defn (.*)/) {
      $cur_chunk = $1; push @chunks, $cur_chunk; next;
    }
    $ancestor{$1} = $cur_chunk, next if (/^\@use (.*)/);
  }
}
foreach $word (@keywords) {
  $is_keyword{$word} = 1;
}
foreach $chunk (@chunks) {
   next if $settled{$chunk};
  $c = $chunk; @chunks_to_settle = ();
  while ((! $settled{$c}) && ($ancestor{$c})) {
    push @chunks_to_settle, $c;
    $c = $ancestor{$c};
  }
  if ($ancestor{$c}) { $root = $ancestor{$c}; }
  else {
    $root = $c;
    $ancestor{$root} = $root;
    $settled{$root} = 1;
  }
  foreach $c (@chunks_to_settle) {
    $ancestor{$c} = $root;
    $settled{$c} = 1;
  }
}
seek (STDIN, 0, 0);
while (<>) {
  if ($incode) {
    if (/\@text (.*)/) { $text .= $1; }
    elsif (/\@nl/ || /\@end/) {
      if (length $text)  { print "\@literal " . pp_line ($text) . "\n"; }
      if (length $extra) { print "$extra"; }
      print $_;
      $text = $extra = "";
      $incode = 0 if (/\@end/);
    } else {
      1 while s/((?:\@use|\@defn) .*?)\[\[(.*?)\]\](?!\])(.*)/$1 . pp_line($2) . $3/e;
      s/(\@index defn )(\S+)/$1 . pp_line($2)/e;
      $extra .= $_;
    }
  } else {
    if (/^\@text %keyword/) {
      $nextline = (<>);
      die "%keyword confusion\n" if (! ($nextline =~ /^\@nl$/));
      print "\@index nl\n";
      next;
    }
    if ($delay && /^\@end docs/) {
      print $texdefs;  $delay = 0;
    }
    1 while s/((?:\@xref chunkbegin|\@defn) .*?)\[\[(.*?)\]\](?!\])(.*)/$1 . pp_line($2) . $3/e;
    print $_;
    if (/^\@quote/ || (/^\@defn (.*)/ && (! $plain{$ancestor{$1}}))) {
      $incode = 1;
      print "\@literal \\Rm{}\n";
    }
  }
}
sub pp_word {
  my ($this_word) = shift;
  if (defined $is_keyword{$this_word}) { return "{\\bf{}$this_word}"; }
  else { return "{\\it{}$this_word}"; }
}
sub pp_line {
  my ($line) = shift;  my ($preline, $postline, $token, $seen_token);
  if ($line =~ /^(\s*)#(\s*\S*)((\s*)(.*))/) {
    $arg = escape ($3);
    $preline = "{\\bf{}$1\\char35{}$2}{\\tt{}$arg}";
    $line = "";
  }
  while ($line =~ m{
                 (.*?)                      #1
                 (                          #2
                   (                        #3 double-quoted string
                    " (\\.|[^\\\"])* "      #4
                   )
                  |
                   (                        #5 single-quoted string
                    ' (\\.|[^\\\']) '       #6
                   )
                  |
                   (?:                      #7 C comment
                     (?: \(\*) (.*?) (?: \*\))
                   )
                  |
                   (                        #8 C++ comment
                    //.*
                   )
                 )
                 (.*)                       #9
                }x) {
#pad:                      (?: /\*) (.*?) (?: \*/)

    if ($3 || $5) {
      $before = $1; $string = $3 || $5; $after = $9;
      $string = escape($string, 1);
      $preline .= pp_line ($before) . "{\\tt{}$string}";
      $line = $after;
      next;
    }
    if ($7) {
      $before = $1; $comment = $7; $after = $9;
      $preline .= pp_line ($before);
      if ($compress_whitespace && ($before =~ /\S/)) { $preline .= "    " }
      $preline .= "{\\commopen}" . pp_comment ($comment) . "{\\commclose}";
      if ($compress_whitespace && ($after =~ /\S/)) { $preline .= "    " }
      $line = $after;
      next;
    }
    if ($8) {
      $line = $1; $postline = $8;
      $postline = pp_comment ($postline);
      if ($compress_whitespace && ($line =~ /\S/)) { $postline = "    " . $postline }
      next;
    }
    die "comment/string confusion\n"; # no match, impossible
  }
  while (length $line) {
    if ($line =~ /^(\s+)(.*)/) {
      if ($compress_whitespace && $seen_token) {
        $preline .= " ";  $line = $2;  next;
      } else {
        $preline .= $1;  $line = $2;  next;
      }
    }
    if ($line =~ /^(0[xX][\dabcdefABCDEF]+)(.*)/) {
      $preline .= $1;  $line = $2;  next;
    }
    if ($line =~ /^(\d+)(.*)/) {
      $preline .= $1;  $line = $2;  next;
    }
    if ($line =~ /^([a-zA-Z\d_]+)(.*)/) {
      $token = $1;  $line = $2;
      ($token = pp_word ($token)) =~ s|_|\\_|g;
      $preline .= $token;
      next;
    }
    if ($line =~ /^([^\d\sa-zA-Z_]+)(.*)/) {
      $token = $1;  $line = $2;  $preline .= pp_punc ($token);  next;
    }
  } continue {
    $seen_token = 1;
  }
  return $preline . $postline;
}
sub init_punc {
  reg_punc ("!=", "${bm}\\neq${em}");
  reg_punc ("&&", "${bm}\\wedge${em}");
  reg_punc ("++", "\\protect\\PP");
  reg_punc ("--", "\\protect\\MM");
  reg_punc ("->", "${bm}\\rightarrow${em}");
  reg_punc ("<<", "${bm}\\ll${em}");
  reg_punc ("<=", "${bm}\\leq${em}");
  reg_punc ("==", "${bm}\\equiv${em}");
  reg_punc (">=", "${bm}\\geq${em}");
  reg_punc (">>", "${bm}\\gg${em}");
  reg_punc ("||", "${bm}\\vee${em}");

  reg_punc ("!", "${bm}\\neg${em}");
  reg_punc ("*", "${bm}\\ast${em}");
  reg_punc ("/", "${bm}\\div${em}");
  reg_punc ("<", "${bm}<${em}");
  reg_punc (">", "${bm}>${em}");
  reg_punc ("^", "${bm}\\oplus${em}");
  reg_punc ("|", "${bm}\\mid${em}");
  reg_punc ("~", "${bm}\\sim${em}");

  reg_punc ("{", "{\\nwlbrace}");
  reg_punc ("}", "{\\nwrbrace}");
}
sub reg_punc {
  my ($punc, $set) = @_;
  push @puncs, $punc;
  $punc_map{$punc} = $set;
}
sub pp_punc {
  my ($this_punc) = shift;  my ($out);
punc_loop:
  while (length $this_punc) {
    foreach $punc (@puncs) {
      if (substr ($this_punc, 0, length $punc) eq $punc) {
        $out .= $punc_map{$punc};
        $this_punc = substr ($this_punc, length $punc);
        next punc_loop;
      }
    }
    # No match found
    $out .= substr ($this_punc, 0, 1);
    $this_punc = substr ($this_punc, 1);
  }
  return $out;
}
sub escape {
  my ($this_line) = shift;  my ($in_tt) = shift;
  if ($in_tt) {
    $this_line =~ s|\\|\001\\char92\002|g;
    $this_line =~ s|{|\001\\char123\002|g;
    $this_line =~ s|}|\001\\char125\002|g;
  } else {
    local ($bm) = "\\begin\001math\002";
    local ($em) = "\\end\001math\002";
    $this_line =~ s|\\|${bm}\\backslash${em}|g;
    $this_line =~ s!\|!${bm}\\mid${em}!g;
    $this_line =~ s|<|${bm}<${em}|g ;
    $this_line =~ s|>|${bm}>${em}|g ;
    $this_line =~ s|{|\001\\nwlbrace\002|g;
    $this_line =~ s|}|\001\\nwrbrace\002|g;
  }
  $this_line =~ s|_|\\_|g ;
  $this_line =~ s|#|\001\\char35\002|g ;
  $this_line =~ s|\001|{|g;
  $this_line =~ s|\002|}|g;
  return $this_line;
}
sub pp_comment {
  my ($this_comment) = shift;
  my ($pre, $code, $post);
  if ($this_comment =~ /(.*?)\[\[(.*?)\]\](?!\])(.*)/) {
    $pre = $1; $code = $2; $post = $3;
    if (defined $tex) {
      return $begcomm . $pre . $endcomm . pp_line ($code) . pp_comment ($post);
    } else {
      return escape ($pre) . pp_line ($code) . pp_comment ($post);
    }
  } else {
    if (defined $tex) {
      return $begcomm . $this_comment . $endcomm;
    } else {
      return escape ($this_comment);
    }
  }
}
