%define debug_package %{nil}
%define _lib_home /usr/lib/%{_name}
%define _build_id_links none

Name: %{_package_name}
Version: %{_version}
Release: %{_release}%{?dist}
Summary: hamler
Group: System Environment/Daemons
License: Apache License Version 2.0
URL: https://www.hamler-lang.org
BuildRoot: %{_tmppath}/%{_name}-%{_version}-root
Provides: %{_name}
AutoReq: 0

%description
Hamler - Haskell-style functional programming language running on Erlang VM. Hamler is a strongly-typed language with compile-time typechecking and built-in support for concurrency and distribution.

%prep

%build

%install
mkdir -p %{buildroot}%{_lib_home}

cp -r %{_srcdir}/bin  %{buildroot}%{_lib_home}/
cp -r %{_srcdir}/ebin %{buildroot}%{_lib_home}/
cp -r %{_srcdir}/lib  %{buildroot}%{_lib_home}/

%pre

%post
if [ $1 = 1 ]; then
    ln -s %{_lib_home}/bin/hamler %{_bindir}/hamler
fi

%preun

%files
%defattr(-,root,root)
%{_lib_home}

%clean
rm -rf %{buildroot}

%changelog

