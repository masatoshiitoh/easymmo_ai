#!/bin/sh

# sudo apt-get update && sudo apt-get -y install git && git clone https://github.com/masatoshiitoh/easymmo.git && cd easymmo && sh devsetup.sh

ERL_DEST=$HOME/otp_R16B03-1
REBAR_DEST=$HOME/rebar


## Install Erlang/OTP
cd $HOME
sudo apt-get update
sudo apt-get -y install apt-transport-https
sudo apt-get -y install wget make
sudo apt-get -y install libc6-dev gcc g++ zlib1g-dev
sudo apt-get -y install build-essential libncurses5-dev libssl-dev
wget http://download.basho.co.jp.cs-ap-e2.ycloud.jp/otp/download/otp_src_R16B03-1.tar.gz
tar xvf otp_src_R16B03-1.tar.gz
cd otp_src_R16B03-1
./configure -prefix=$ERL_DEST
make
sudo make install
echo "PATH=$ERL_DEST/bin:\$PATH" >> $HOME/.profile
PATH=$ERL_DEST/bin:$PATH


