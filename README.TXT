mypaedia-package --- FreePWING �Łw�}�C�y�f�B�A�x�p lookup �⏕�p�b�P�[�W

    �����Ђ� <kazuhiko@ring.gr.jp>
    http://openlab.ring.gr.jp/edict/mypaedia-fpw/

���̃f�B���N�g���ɂ̓}�C�y�f�B�A�̃}���`���f�B�A�E�f�[�^�� lookup �ŗ��p
���邽�߂� elisp �t�@�C�������߂Ă��܂��B

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
1. �K�v�Ȃ���

    FreePWING �Ń}�C�y�f�B�A 1.3.2 �ȍ~
	http://openlab.ring.gr.jp/edict/mypaedia-fpw/
	��҂̏��L����w�}�C�y�f�B�A for Win (2000�N��)�x�A�w�}�C�y�f�B
	�A PC Success �Łx�œ���m�F�ρB

    lookup 1.2 (�����炭 1.0 �� 1.1 �ł���)
	http://openlab.ring.gr.jp/lookup/

���̂ق��CJPEG�CBMP�CPNG�CMIDI�CWAV�CAVI ��\���E�Đ��ł���\�t�g�E�F�A
���K�v�ł� (�S���͂Ȃ��Ă���)�B

(�Q�l URL)

    TiMidity++ -- MIDI �Đ�
	http://www.goice.co.jp/member/mo/timidity/
    sox -- WAV �Đ�
	http://home.sprynet.com/~cbagwell/sox.html
    xanim -- AVI �Đ� (cinepak �p�� dll ���K�v)
	http://xanim.va.pubnix.com/home.html
    ImageMagick -- �e��摜�̕\��
	http://www.wizards.dupont.com/cristy/ImageMagick.html

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
2. ���p���@

~/.lookup �ŁC

   (setq lookup-search-agents '((ndeb "/usr/share/dict/mypaedia")))
   (setq lookup-package-directory "/usr/share/dict/package")
   (lookup-use-package "ndeb+/usr/share/dict/mypaedia" "mypaedia-fpw")

   ; BH.DAT�CCH.DAT�CMAP.DAT�CPICT.DAT �̂���ꏊ
   (setq mypaedia-data-directory "/mnt/cdrom/INDEX")

   ; *.MID�C*.WAV�C*.AVI �̂���ꏊ
   (setq mypaedia-av-directory "/mnt/cdrom/DATA")

   (setq mypaedia-play-midi-process "timidity")
   (setq mypaedia-play-wav-process "soxplay")
   (setq mypaedia-play-avi-process "xanim")
   (setq mypaedia-display-image-process "display")
   ; "perl �̃v���Z�X���B�p�X���ʂ��Ă��Ȃ��ꍇ�̓t���p�X�ŋL�q���邱�ƁB
   (setq perl-process "perl")

   ; �ꎞ�t�@�C���̒u���ꏊ�B
   (setq temorary-file-directory "/tmp")
 
�̂悤�ɐݒ肵�āClookup-package-directory �� mypaedia-fpw.el �� 
extract-mypaedia.pl ���R�s�[���ĉ������Bxemacs ����� emacs-21 �ł͉摜
�̃C�����C���\���ɑΉ����Ă��܂����Cxemacs �ł̓L���b�V���������Ă��Ȃ�
�̂ŁC�摜���ĕ\���������Ƃ��ɂ� `g' �Ń����[�h����K�v������܂��B�Ȃ��C
�C�����C���\�����������Ȃ��Ƃ��́C~/.lookup ��

   (setq mypaedia-image-inline nil)

�Ƃ��Ă��������B�܂��C

�摜�CMIDI�CWAV�CAVI �̂����\���E�Đ��������Ȃ����̂ɂ��ẮC
~/.lookup �ł��ꂼ��

   (setq mypaedia-display-image-process nil)
   (setq mypaedia-play-midi-process nil)
   (setq mypaedia-play-wav-process nil)
   (setq mypaedia-play-avi-process nil)

�Ƃ��Ă��������B

�����⓮����C�����Ɠ����ɍĐ��������ꍇ�́C~/.lookup �ł��ꂼ��

   (setq mypaedia-sound-without-notice t)
   (setq mypaedia-video-without-notice t)

�Ƃ��Ă��������B

�Ȃ� lookup �Ō�������ۂɁA�f�[�^�ʂ̑����G���g���Ŗ{�����r���� <more
point=5> �ƂȂ��ēr�؂��ꍇ�́Alookup-max-text �̒l��ύX���Ă��������B
�ڂ����� lookup �t���̃h�L�������g���������������B

> lookup-max-text : �������ɕ\������G���g���{���̍ő咷�B0 ���w�肷��ƁA
> �S����\������B

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
2. ���C�Z���X

mypaedia-package �̓t���[�\�t�g�E�F�A�ł��B���Ȃ��́CFree Software
Foundation �����\���� GNU General Public License (GNU ��ʌ��L�g�p����)
�o�[�W���� 2 ���邢�͂���ȍ~�̊e�o�[�W�����̒����炢���ꂩ��I�����C��
�̃o�[�W��������߂�����ɏ]���Ė{�v���O�������ĔЕz�܂��͕ύX���邱�Ƃ�
�ł��܂��B

mypaedia-package �͗L�p�Ƃ͎v���܂����C�Еz�ɂ������ẮC�s�ꐫ�y�ѓ���
�ړI�K�����ɂ��Ă̈Öق̕ۏ؂��܂߂āC�����Ȃ�ۏ؂��s���܂���B�ڍׂ�
���Ă� GNU General Public License �����ǂ݉������B

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
3. �ӎ�

���^������Ă������������c�\���C�啝�Ɋg�����Ă������������}�a�`����C
����m�F�Ȃǂɂ����͂�����������{������AOKUNISHI ����A������ lookup
Mailing List *1 �݂̂Ȃ���Ɋ��ӂ��܂��B

 *1 http://openlab.ring.gr.jp/lookup/