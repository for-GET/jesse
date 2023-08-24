# CHANGES

## develop

## 2023.1.1

- [FIX] efmt 用に入れていた `-ifdef` の使い方が間違っていたのを修正する
  - @sile
  
## 2023.1.0

- [CHANGE] hex のパッケージ名を shiguredo_jesse に変更する
  - @sile
- [UPDATE] efmt を適用する
  - @sile
- [FIX] OTP の master ブランチでコンパイル可能にする
  - 0.0 にパターンマッチする箇所で警告が出ていたので `==` での比較に置換した 
  - @sile
