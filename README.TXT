mypaedia-package --- FreePWING 版『マイペディア』用 lookup 補助パッケージ

    かずひこ <kazuhiko@ring.gr.jp>
    http://openlab.ring.gr.jp/edict/mypaedia-fpw/

このディレクトリにはマイペディアのマルチメディア・データを lookup で利用
するための elisp ファイルを収めています。

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
1. 必要なもの

    FreePWING 版マイペディア 1.3.2 以降
	http://openlab.ring.gr.jp/edict/mypaedia-fpw/
	作者の所有する『マイペディア for Win (2000年版)』、『マイペディ
	ア PC Success 版』で動作確認済。

    lookup 1.2 (おそらく 1.0 や 1.1 でも可)
	http://openlab.ring.gr.jp/lookup/

そのほか，JPEG，BMP，PNG，MIDI，WAV，AVI を表示・再生できるソフトウェア
が必要です (全部はなくても可)。

(参考 URL)

    TiMidity++ -- MIDI 再生
	http://www.goice.co.jp/member/mo/timidity/
    sox -- WAV 再生
	http://home.sprynet.com/~cbagwell/sox.html
    xanim -- AVI 再生 (cinepak 用の dll も必要)
	http://xanim.va.pubnix.com/home.html
    ImageMagick -- 各種画像の表示
	http://www.wizards.dupont.com/cristy/ImageMagick.html

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
2. 利用方法

~/.lookup で，

   (setq lookup-search-agents '((ndeb "/usr/share/dict/mypaedia")))
   (setq lookup-package-directory "/usr/share/dict/package")
   (lookup-use-package "ndeb+/usr/share/dict/mypaedia" "mypaedia-fpw")

   ; BH.DAT，CH.DAT，MAP.DAT，PICT.DAT のある場所
   (setq mypaedia-data-directory "/mnt/cdrom/INDEX")

   ; *.MID，*.WAV，*.AVI のある場所
   (setq mypaedia-av-directory "/mnt/cdrom/DATA")

   (setq mypaedia-play-midi-process "timidity")
   (setq mypaedia-play-wav-process "soxplay")
   (setq mypaedia-play-avi-process "xanim")
   (setq mypaedia-display-image-process "display")
   ; "perl のプロセス名。パスが通っていない場合はフルパスで記述すること。
   (setq perl-process "perl")

   ; 一時ファイルの置き場所。
   (setq temorary-file-directory "/tmp")
 
のように設定して，lookup-package-directory に mypaedia-fpw.el と 
extract-mypaedia.pl をコピーして下さい。xemacs および emacs-21 では画像
のインライン表示に対応していますが，xemacs ではキャッシュが効いていない
ので，画像を再表示したいときには `g' でリロードする必要があります。なお，
インライン表示させたくないときは，~/.lookup で

   (setq mypaedia-image-inline nil)

としてください。また，

画像，MIDI，WAV，AVI のうち表示・再生したくないものについては，
~/.lookup でそれぞれ

   (setq mypaedia-display-image-process nil)
   (setq mypaedia-play-midi-process nil)
   (setq mypaedia-play-wav-process nil)
   (setq mypaedia-play-avi-process nil)

としてください。

音声や動画を，検索と同時に再生したい場合は，~/.lookup でそれぞれ

   (setq mypaedia-sound-without-notice t)
   (setq mypaedia-video-without-notice t)

としてください。

なお lookup で検索する際に、データ量の多いエントリで本文が途中で <more
point=5> となって途切れる場合は、lookup-max-text の値を変更してください。
詳しくは lookup 付属のドキュメントをご覧ください。

> lookup-max-text : 検索時に表示するエントリ本文の最大長。0 を指定すると、
> 全文を表示する。

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
2. ライセンス

mypaedia-package はフリーソフトウェアです。あなたは，Free Software
Foundation が公表した GNU General Public License (GNU 一般公有使用許諾)
バージョン 2 あるいはそれ以降の各バージョンの中からいずれかを選択し，そ
のバージョンが定める条項に従って本プログラムを再頒布または変更することが
できます。

mypaedia-package は有用とは思いますが，頒布にあたっては，市場性及び特定
目的適合性についての暗黙の保証を含めて，いかなる保証も行いません。詳細に
ついては GNU General Public License をお読み下さい。

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
3. 謝辞

原型を作ってくださった西田圭介さん，大幅に拡張してくださった是枝和義さん，
動作確認などにご協力くださった野宮賢さん、OKUNISHI さん、そして lookup
Mailing List *1 のみなさんに感謝します。

 *1 http://openlab.ring.gr.jp/lookup/
