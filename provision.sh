sudo apt-get update
sudo apt-get dist-upgrade -qy

# install erlang
echo "deb http://packages.erlang-solutions.com/debian jessie contrib" | sudo tee /etc/apt/sources.list.d/erlang.list
wget -q -O- "http://packages.erlang-solutions.com/debian/erlang_solutions.asc" | sudo apt-key add -
sudo apt-get update
sudo apt-get install erlang git build-essential -qy

# install nodejs
NODE_VERSION=v6.9.1
curl -sSOL https://nodejs.org/dist/$NODE_VERSION/node-$NODE_VERSION-linux-x64.tar.gz
mkdir /opt/nodejs
tar -xzf node-$NODE_VERSION-linux-x64.tar.gz -C /opt/nodejs
rm node-$NODE_VERSION-linux-x64.tar.gz
ln -s /opt/nodejs/node-$NODE_VERSION-linux-x64/bin/node /usr/bin/node
ln -s /opt/nodejs/node-$NODE_VERSION-linux-x64/bin/npm /usr/bin/npm

# install elm
npm install --global elm
