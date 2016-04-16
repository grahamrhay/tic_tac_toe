# -*- mode: ruby -*-
# vi: set ft=ruby :

VAGRANTFILE_API_VERSION = "2"

Vagrant.configure(VAGRANTFILE_API_VERSION) do |config|
  config.vm.box = "debian8"

  config.vm.provision "shell", path: "provision.sh"

  config.vm.network "private_network", ip: "192.168.50.4"
  config.vm.network :forwarded_port, :host => 8888, :guest => 8888

  config.vm.provider "virtualbox" do |v|
    v.memory = 1024
  end
end
