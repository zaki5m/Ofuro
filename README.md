# Ofuro
副露率を指定することができる麻雀AI"Ofuro"．手牌の期待値と和了率を副露の判断基準にすることで副露率を可変にすることを可能にしている．

現状は10%，15%，20%，25%，30%，35%を指定可能．(35%はほぼ31%で動作)

<h1>環境</h1>
ocaml >= 5.0.0

<h1>buildとシミュレーション実行方法</h1>
port番号を指定しplayerそれぞれの副露率を入力すると2000局のシミュレーションが開始される．
<pre><code>$ git clone https://github.com/zaki5m/Ofuro.git
$ dune build
$ ./_build/default/mahjong_admin.exe port player1 player2 player3 player4
</pre></code>

## mjaiでのシミュレーション

麻雀AIサーバ"mjai"で動作する麻雀AI．

mjai : https://github.com/gimite/mjai


![image](https://user-images.githubusercontent.com/86464909/243076638-a025c829-a6cf-405c-9063-a618f167e452.png)
