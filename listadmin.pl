#! /local/bin/perl -w

use HTML::TokeParser;
use LWP::Simple;
use Data::Dumper;
use Term::ReadLine;
use strict;

my $rc = $ENV{"HOME"}."/.listadminrc";
my $oldconf = $ENV{"HOME"}."/.listconf";
upgrade_config($oldconf, $rc);

if (@ARGV >= 2 && $ARGV[0] eq "-f") {
    shift; $rc = shift;
}
if (@ARGV != 0) {
    print STDERR "Usage: $0 [-f CONFIGFILE]\n";
    exit (64);
}

my $config = read_config ($rc);

unless ($config) {
    print STDERR "No configuration file!\n";
    exit (1);
}

my ($info, $id);

format STDOUT =
From:    @<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
         $info->{$id}{"from"}
Subject: ^<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
         $info->{$id}{"subject"}
~~       ^<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
         $info->{$id}{"subject"}
Reason:  @<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  Spam? @<<
         $info->{$id}{"reason"},               $info->{$id}{"spamscore"}
.

my $term = new Term::ReadLine 'listadmin';
my $prompt =
'Approve/Reject/Discard/Skip/view Body/view Full/Help/Quit/eXit ? ';

for my $list (sort {$config->{$a}{"order"} <=> $config->{$b}{"order"}}
	           keys %{$config}) {
    my $def = $config->{$list}{"default"};
    my $spamlevel = $config->{$list}{"spamlevel"};
    my $user = $config->{$list}{"user"};
    my $pw = $config->{$list}{"password"};

    print "fetching data for $list\n";
    $info = get_list ($list, $user, $pw);
    my $num = 0;
    my $count = scalar keys %{$info};
    my %change = ();

 msgloop:
    for $id (sort keys %{$info}) {
	++$num;
	print "\n[$num/$count] ======= #$id of $list =======\n";
	write;

	while (1) {
	    my $ans;
	    if ($spamlevel && $info->{$id}{"spamscore"} >= $spamlevel) {
		print "Automatically discarded as spam.\n";
		$ans = "d";
	    }
	    $ans ||= $config->{$list}{"action"};
	    $ans ||= $term->readline ($prompt);
	    $ans = "q" unless defined $ans;
	    $ans = $def if $ans eq "";
	    $ans =~ s/\s+//g;
	    $ans = lc $ans;
	    last msgloop if $ans eq "q";
	    next msgloop if $ans eq "s";
	    if ($ans eq "a") {
		$change{$id} = [ "0" ];
		last;
	    } elsif ($ans eq "d") {
		$change{$id} = [ "2" ];
		last;
	    } elsif ($ans eq "r") {
		my $r = $term->readline ("Why do you reject? ");
		$change{$id} = [ "1", $r ];
		last;
	    } elsif ($ans eq "x") {
		%change = ();
		last msgloop;
	    } elsif ($ans eq "f") {
		print $info->{$id}{"excerpt"};
	    } elsif ($ans eq "b") {
		my $text = $info->{$id}{"excerpt"};
		$text =~ s/.*?\n\n//s;
		my @lines = split (/\n/, $text, 21);
		pop @lines;
		print join ("\n", @lines), "\n";
	    } elsif ($ans eq "") {
		# nothing.
	    } else {
		print <<"end";
Choose one of the following actions by typing the corresponding letter
and pressing Return.

  a  Approve   -- the message will be sent to all member of the list
  r  Reject    -- notify sender that the message was rejected
  d  Discard   -- throw message away, don't notify sender
  s  Skip      -- don't decide now, leave it for later
  b  view Body -- display the first 20 lines of the message
  f  view Full -- display the complete message, including headers
  q  Quit      -- go on to the next list
  x  eXit      -- go on to the next list, undo all actions chosen

end
		print <<"end" if $def;
The default action for this list when you only press Return is '$def'

end
            }
	}
    }
    commit_changes ($list, $user, $pw, \%change);
}

sub mailman_url {
    my ($list, $user, $pw) = @_;

    $pw =~ s/(\W)/sprintf("%%%02x", ord($1))/ge;

    my $args = "username=$user&adminpw=$pw"
    my ($lp, $domain) = split ('@', $list);
    my $www;
    if ($domain eq "lister.ping.uio.no") {
	return "https://$domain/mailman/$domain/admindb/$lp?$args";
    } elsif ($domain =~ /^(\w+)\.uio\.no$/) {
	$www = "$1-lists.uio.no";
    } elsif ($domain eq "uio.no") {
	$www = "uio-lists.uio.no";
    } else {
	$www = "lister.uio.no";
    }
    return "http://$www/mailman/admindb/$list?$args";
}

sub get_list {
    my ($list, $user, $pw) = @_;

    # where we gather all the information about pending messages
    my %data = ();

    my $page = get (mailman_url ($list, $user, $pw));

    my $parse = HTML::TokeParser->new(\$page) || die;
    my ($from, $subject, $reason, $id, $tag, $excerpt, $spamscore);
    while ($parse->get_tag ("table")) {
    
	$parse->get_tag ("tr") || die; # From:
	$parse->get_tag ("td") || die;
	$parse->get_tag ("td") || die;
	$from = $parse->get_trimmed_text("/td");

	$parse->get_tag ("tr") || die; # Reason:
	$parse->get_tag ("td") || die;
	$parse->get_tag ("td") || die;
	$reason = $parse->get_trimmed_text("/td");

	$parse->get_tag ("tr") || die; # Subject:
	$parse->get_tag ("td") || die;

	# the parsing just happens to fail here when the wrong
	# password is given...
	$parse->get_tag ("td") ||
		die "Parse failed.  Is your username and password correct?\n";
	$subject = $parse->get_trimmed_text("/td");

	$parse->get_tag ("tr") || die; # Action:
	$tag = $parse->get_tag ("input") || die;
	$id = $tag->[1]{"name"};

	$parse->get_tag ("tr") || die; # Reject text area

	$parse->get_tag ("tr") || die; # Message Excerpt:
	$parse->get_tag ("td") || die;
	$parse->get_tag ("td") || die;
	$excerpt = $parse->get_text("/td");
	$excerpt =~ /^X-UiO-Spam-score: (s+)/m;
	$spamscore = length ($1 || "");

	$parse->get_tag ("/table") || die;

	$data{$id} = { "from" => $from,
		       "subject" => $subject,
		       "reason" => $reason,
		       "spamscore" => $spamscore,
		       "excerpt" => $excerpt };
    }
    return \%data;
}

sub upgrade_config {
    my ($conf, $rc) = @_;
    return if -f $rc;
    return unless -f $conf;

    print "Converting to new configuration file, $rc\n\n";

    my $cmd = ". $conf; umask 077; > $rc ". <<'END';
(echo "# automatically converted from .listconf";
 echo "#";
 echo "username $LISTUSER";
 echo "password \"$LISTPASS\"";
 echo "spamlevel 15";
 echo "default discard";
 echo ""
 for l in $LISTS; do echo "$l"; done
)
END
    system $cmd;
}

sub read_config {
    my ($file) = @_;

    my ($user, $pw, $spam, $list);
    my %conf = ();
    my $line = "";
    my $action = "";
    my $default = "";
    my $count = 0;
    my $lineno = 0;
    
    my %act = ("approve" => "a", "discard" => "d",
	       "reject" => "r", "skip" => "s", "none" => "");

    return undef unless open (CONF, $file);
    while (<CONF>) {
	++$lineno;
	chomp;
	next if /^\s*#/;
	if (/\\$/) {
	    $line = $`; # $PREFIX
	    next;
	}
	$line .= $_;
	$line =~ s/^\s+//;
	next if /^$/;
	if ($line =~ /^username\s+/i) {
	    $user = $'; # $POSTFIX
	    $user =~ s/\s+$//;
	    $user =~ s/^"(.*)"$/$1/;
	    if ($user !~ /^[a-z0-9-]+\@[a-z0-9.-]+$/) {
		print STDERR "$file:$lineno: Illegal username: '$user'\n";
		exit 1;
	    }
	} elsif ($line =~ /^password\s+/i) {
	    $pw = $'; # $POSTFIX
	    $pw =~ s/\s+$//;
	    if ($pw =~ /^"(.*)"$/) {
		$pw = $1;
		$pw =~ s/\\"/"/g;
		$pw =~ s/\\\\/\\/g;
	    }
	} elsif ($line =~ /^spamlevel\s+/i) {
	    $spam = $';
	    if ($spam =~ /^(\d+)\s*$/) {
		$spam = $1;
	    } else {
		print STDERR "$file:$lineno: Illegal spamlevel value: '$spam'\n";
		exit 1;
	    }
	} elsif ($line =~ /^action\s+/i) {
	    $action = $'; # $POSTFIX
	    $action =~ s/^"(.*)"\s*/$1/;
	    unless (defined $act{$action}) {
		print STDERR "$file:$lineno: Illegal action value: '$action'\n";
		exit 1;
	    }
	    $action = $act{$action};
	} elsif ($line =~ /^default\s+/i) {
	    $default = $'; # $POSTFIX
	    $default =~ s/^"(.*)"\s*/$1/;
	    unless (defined $act{$default}) {
		print STDERR "$file:$lineno: Illegal default value: '$default'\n";
		exit 1;
	    }
	    $default = $act{$default};
	} elsif ($line =~ /^([^@ \t]+@[^@])+\s*/) {
	    $conf{$line} = { "user" => $user,
			     "password" => $pw,
			     "spamlevel" => $spam,
			     "action" => $action,
			     "default" => $default,
			     "order" => ++$count,
			 };
	} else {
	    print STDERR "$file:$lineno: Syntax error: '$line'\n";
	    exit 1;
	}
	$line = "";
    }
    close (CONF);
    return \%conf;
}

sub commit_changes {
    my ($list, $user, $pw, $change) = @_;

    my $url = mailman_url ($list, $user, $pw);
    my $changes = 0;

    for $id (keys %{$change}) {
	my ($what, $text) = @{$change->{$id}};
	$url .= "&$id=$what";
	if ($what == 1) {
	    $text =~ s/(\W)/sprintf("%%%02x", ord($1))/ge;
	    $url .= "&comment_$id=$text";
	}
	++$changes;
    }
    get ($url) if $changes;
}
