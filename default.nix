{ pkgs ? import <nixpkgs> {} }:
(pkgs.haskellPackages.callPackage ./hyperpipe.nix {}).overrideAttrs (hp: rec {
  postInstall = ''
    mkdir -p $out/etc/dbus-1/system.d
    cat <<EOF > $out/etc/dbus-1/system.d/com.hyperpipe.conf
<?xml version="1.0"?> <!--*-nxml-*-->
<!DOCTYPE busconfig PUBLIC "-//freedesktop//DTD D-BUS Bus Configuration 1.0//EN"
	"http://www.freedesktop.org/standards/dbus/1.0/busconfig.dtd">
<busconfig>
	<policy user="root">
		<allow own="com.hyperpipe"/>
	</policy>
	<policy context="default">
		<allow send_destination="com.hyperpipe"/>
		<allow receive_sender="com.hyperpipe"/>
	</policy>
</busconfig>
EOF
  '';
})
