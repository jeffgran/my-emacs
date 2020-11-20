# install bash and emacs
brew install bash
brew cask install emacs

# setup bash profile
brew install rbenv
brew install bash-git-prompt
cd ~/dev
git clone git@github.com:jeffgran/my-bash.git
cd my-bash
ln -s $HOME/dev/my-bash/.bash_profile $HOME/.bash_profile
ln -s $HOME/dev/my-bash/.bashrc $HOME/.bashrc


# install emacs cask (package manager)
curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python

# emacs extra necessary stuff 
brew tap homebrew/cask-fonts && brew cask install font-iosevka
brew install ag


# osx keybindings
mkdir -p ~/Library/KeyBindings
cp ~/dev/my-bash/DefaultKeyBinding.dict ~/Library/KeyBindings/DefaultKeyBinding.dict
defaults write -g NSTextKillRingSize -string 12
defaults write -g NSRepeatCountBinding -string "^u"

# osx window management
brew cask install rectangle
# set up shortcuts (use spectacle defaults, they are pretty close)

# write default system keybindings (spotlight) ??? don't actually understand this
# more examples here: https://github.com/diimdeep/dotfiles/blob/master/osx/configure/hotkeys.sh
# Spotlight > Show Spotlight search : Shift+Control+Option+S
defaults write com.apple.symbolichotkeys.plist AppleSymbolicHotKeys -dict-add 64 "
  <dict>
    <key>enabled</key><true/>
    <key>value</key><dict>
      <key>type</key><string>standard</string>
      <key>parameters</key>
      <array>
        <integer>115</integer>
        <integer>1</integer>
        <integer>917504</integer>
      </array>
    </dict>
  </dict>
"


# (restart to apply)
 

