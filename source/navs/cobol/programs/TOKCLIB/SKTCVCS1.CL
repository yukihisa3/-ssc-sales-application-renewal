/.****************************************************************./
/.*                                                              *./
/.*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*./
/.*    サブシステム　　　　：　受配信管理システム　　　　　　　　*./
/.*    業務名　　　　　　　：　ＣＶＣＳ管理                      *./
/.*    モジュール名　　　　：　ＣＶＣＳ管理ファイル創成          *./
/.*    作成日／更新日　　　：　99/11/22                          *./
/.*    作成者／更新者　　　：　ＮＡＶ高橋                        *./
/.*    処理概要　　　　　　：　パラメタより、創成するタイブラリ名*./
/.*                            を取得し、ＣＶＣＳ管理ファイルを作*./
/.*                            成する。（ＩＳＤＮ回線用）        *./
/.****************************************************************./
    PGM       (SIZEK-?SIZEK,CLIB-?CLIB)
/.## パラメタ ##./
    PARA      ?SIZEK,INTEGER,IN,VALUE-200
    PARA      ?CLIB ,STRING*8,IN,VALUE-'CVCSLBI'
    VAR       ?SEN  ,STRING*1,VALUE-'I'
/.## ライブラリリスト登録 ##./
    DEFLIBL   LIBL-CVCSLBI/CVCSOBJ/TOKFLIB
/.## ダムファイル作成 ##./
    DLTFILE     FILE-CVCSD003.CVCSLBI
    DLTFILE     FILE-CVCSD006.CVCSLBI
    CRTFILE     FILE-CVCSD003.CVCSLBI,SIZE-8000
    CRTFILE     FILE-CVCSD006.CVCSLBI,SIZE-5000
    CHGFILE     FILE-CVCSD003.CVCSLBI,ORG-@DF,KEY-5!0
    CHGFILE     FILE-CVCSD006.CVCSLBI,ORG-@DF,KEY-5!0
    OVRF        FILE-CVCSD003,TOFILE-CVCSD003.CVCSLBI
    OVRF        FILE-CVCSD006,TOFILE-CVCSD006.CVCSLBI

    CALL        PGM-CVCS1008.CVCSOBJ

/.## ファイルの置換 ##./
    OVRF FILE-CVCSS012,TOFILE-CVCSS012.CVCSLBI
    OVRF FILE-CVCSS013,TOFILE-CVCSS013.CVCSLBI
    OVRF FILE-CVCSS016,TOFILE-CVCSS016.CVCSLBI
    OVRF FILE-KANRIWK,TOFILE-KANRIWK1.TOKFLIB
/.## ＥＯＳ管理Ｆ→取引先管理マスタ作成 ##./
    CALL SCV0011B.TOKELIB

/.* CVCS管理Ｍ創成 *./
    DLTFILE   FILE-CVCSI001.CVCSLBI
    DLTFILE   FILE-CVCSS016.CVCSLBI
    CRTFILE   FILE-CVCSS016.CVCSLBI,SIZE-?SIZEK
    CRTFILE   FILE-CVCSI001.CVCSLBI,SIZE-?SIZEK
    CHGFILE   FILE-CVCSS016.CVCSLBI
    CHGFILE  FILE-CVCSI001.CVCSLBI,ORG-@ISF,BF-1,KEY-12!0,OVFSIZE-10,
              MIBF-6,IBF-6,ABF-1
    SORT       INFILE-CVCSS012.CVCSLBI,INRL-80,
              OUTFILE-CVCSS012.CVCSLBI,KEY-1!12!CA,
              INBF-4,OUTBF-4
    SORT       INFILE-CVCSS013.CVCSLBI,INRL-80,
              OUTFILE-CVCSS013.CVCSLBI,KEY-1!25!CA,
              INBF-4,OUTBF-4
    CALL      PGM-CVCS1007.CVCSOBJ
/.## 管理ファイル創成用ファイルソート ##./
    SORT       INFILE-CVCSS016.CVCSLBI,INRL-500,
              OUTFILE-CVCSX013.CVCSLBI,KEY-1!12!CA,INBF-1,OUTBF-1
    IF        @PGMEC ^= 0   THEN
              SNDMSG MSG-'ABEND',TO-XCTL END
/.## 管理ファイルコピー ##./
    SETISF    FILE-CVCSX013.CVCSLBI,TOFILE-CVCSI001.CVCSLBI
    RETURN    PGMEC-@PGMEC
