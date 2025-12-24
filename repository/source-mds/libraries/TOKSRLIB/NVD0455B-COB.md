# NVD0455B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/NVD0455B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　Ｄ３６５連携　　　　　　　　　　　*
*    モジュール名　　　　：　ＮＡＶＳ入荷分移動ＤＴ更新　　　  *
*    処理概要　　　　　　：　ＮＡＶＳ－ＮＡＶＳ間移動の入荷ＤＴ*
*    　　　　　　　　　　　　より、新入出庫ファイルを更新する。*
*    作成日／作成者　　　：　2020/06/01   ASS.TAKAHASHI        *
*    更新日／更新者　　　：　YYYY/MM/DD                        *
*    　　変更概要　　　　：                                    *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            NVD0455B.
 AUTHOR.                NAV.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU-GP6000.
 OBJECT-COMPUTER.       FUJITSU-GP6000.
 SPECIAL-NAMES.
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
****<< 入庫ファイル >>*************************************
     SELECT   NYKFILF   ASSIGN    TO        DA-01-VI-NYKFILL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   SEQUENTIAL
                        RECORD    KEY       IS   NYU-F02
                                                 NYU-F03
                                                 NYU-F04
                                                 NYU-F05
                        FILE      STATUS    IS   NYU-STATUS.
****<< 発注ファイル >>*************************************
     SELECT   HACMEIF   ASSIGN    TO        DA-01-VI-HACMEIL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   HAM-F02
                                                 HAM-F03
                        FILE      STATUS    IS   HAM-STATUS.
****<< 新入出庫データ >>************************************
     SELECT   DNSFILF   ASSIGN    TO        DA-01-VI-DNSFILL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   DNS-F01
                                                 DNS-F02
                        FILE      STATUS    IS   DNS-STATUS.
***************************************************************
 DATA                   DIVISION.
***************************************************************
 FILE                   SECTION.
***************************************************************
****<< 入庫ファイル >>*************************************
 FD  NYKFILF.
     COPY     NYKFILF   OF        XFDLIB
              JOINING   NYU       PREFIX.
****<< 発注ファイル >>*************************************
 FD  HACMEIF.
     COPY     HACMEIF   OF        XFDLIB
              JOINING   HAM       PREFIX.
****<< 新入出庫データ >>************************************
 FD  DNSFILF.
     COPY     DNSFILF   OF        XFDLIB
              JOINING   DNS       PREFIX.
****  作業領域  ************************************************
 WORKING-STORAGE        SECTION.
****************************************************************
****  ステイタス情報
 01  STATUS-AREA.
     02 NYU-STATUS           PIC  X(02).
     02 HAM-STATUS           PIC  X(02).
     02 RUI-STATUS           PIC  X(02).
     02 DNS-STATUS           PIC  X(02).
****  システム日付・時刻
 01  SYSTEM-HIZUKE.
     03  SYSYMD              PIC  9(06).
     03  WK-SYSDATE          PIC  9(08).
     03  SYSHMS.
         05  WK-SYSTIME      PIC  9(06).
****  フラグ
 01  END-FLG                 PIC  X(03)  VALUE  SPACE.
 01  HACMEIF-INV-FLG         PIC  X(03)  VALUE  SPACE.
 01  DNSFILF-INV-FLG         PIC  X(03)  VALUE  SPACE.
****  カウント
 01  CNT-AREA.
     03  IN-CNT              PIC  9(10)  VALUE  ZERO.
     03  IN-CNT-R            REDEFINES  IN-CNT.
         05  FILLER          PIC  X(07).
         05  IN-CNT-X        PIC  X(03).
     03  SEL-CNT             PIC  9(10)  VALUE  ZERO.
     03  OUT-CNT             PIC  9(10)  VALUE  ZERO.
****  処理件数表示
 01  DSP-CNT-AREA.
     03  DSP-READ-AREA.
         05  FILLER          PIC  X(23) VALUE
         "*** NYKFILL1   (READ) =".
         05  DSP-READ        PIC  Z,ZZZ,ZZZ,ZZ9.
         05  FILLER          PIC  X(15) VALUE
         " * 処理中]]".
     03  DSP-IN-AREA.
         05  FILLER          PIC  X(23) VALUE
         "*** NYKFILL1  (INPUT) =".
         05  DSP-IN          PIC  Z,ZZZ,ZZZ,ZZ9.
     03  DSP-OUT-AREA.
         05  FILLER          PIC  X(23) VALUE
         "*** JISNYKF  (OUTPUT) =".
         05  DSP-OUT         PIC  Z,ZZZ,ZZZ,ZZ9.

**** メッセージ情報           ****
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "NVD0455B".
       03  FILLER            PIC  X(10)  VALUE
          " ABEND ###".
     02  MSG-ABEND2.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-FL-ID         PIC  X(08).
       03  FILLER            PIC  X(04)  VALUE  " ST-".
       03  ERR-STCD          PIC  X(02).
       03  FILLER            PIC  X(04)  VALUE  " ###".
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
****************************************************************
****************************************************************
 PROCEDURE                   DIVISION.
****************************************************************
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  NYKFILF.
     MOVE   "NYKFILL1"        TO    ERR-FL-ID.
     MOVE    NYU-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC2           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  HACMEIF.
     MOVE   "HACMEIL1"        TO    ERR-FL-ID.
     MOVE    HAM-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC3           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  DNSFILF.
     MOVE   "DNSFILL1"       TO    ERR-FL-ID.
     MOVE    DNS-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
 END     DECLARATIVES.
************************************************************
*             基本処理
************************************************************
 PGM-CONTROL                     SECTION.
     PERFORM           100-INIT-SEC.
     PERFORM           200-MAIN-SEC
                            UNTIL     END-FLG   =    "END".
     PERFORM           300-END-SEC.
     STOP     RUN.
 PGM-CONTROL-EXT.
     EXIT.
************************************************************
*      １００   初期処理
************************************************************
 100-INIT-SEC           SECTION.
*システム日付・時刻の取得
     ACCEPT   SYSYMD     FROM     DATE.
     ACCEPT   SYSHMS     FROM     TIME.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     SYSYMD              TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE     LINK-OUT-YMD        TO   WK-SYSDATE.

     OPEN     I-O       NYKFILF.
     OPEN     I-O       HACMEIF.
     OPEN     I-O       DNSFILF.
*
     MOVE     SPACE               TO   NYU-REC.
     INITIALIZE                        NYU-REC.
*
     MOVE     7000000             TO   NYU-F02.
     START  NYKFILF  KEY  IS  >=  NYU-F02  NYU-F03  NYU-F04
                                  NYU-F05
            INVALID
            DISPLAY NC"＃＃対象ＤＴ無（ＳＴ）＃＃" UPON CONS
            MOVE    "END"         TO   END-FLG
            GO                    TO   100-INIT-END
     END-START.
*

*入庫ファイルの読込
     PERFORM  NYKFILF-READ-SEC.

 100-INIT-END.
     EXIT.
************************************************************
*      ２００   主処理
************************************************************
 200-MAIN-SEC           SECTION.

*発注明細ファイル／検索
     MOVE     NYU-F02         TO   HAM-F02.      *> NAVS-NO
     MOVE     NYU-F05         TO   HAM-F03.      *> 行番号
     PERFORM  HACMEIF-READ-SEC.
     IF  HACMEIF-INV-FLG  =  "INV"
         DISPLAY "＃＃＃　発注明細ファイルなし KEY="
                  HAM-F02 "," HAM-F03   UPON  CONS
         MOVE    4000             TO    PROGRAM-STATUS
         STOP    RUN
     END-IF.

*新入出庫ファイル／検索
     MOVE     NYU-F02         TO   DNS-F01.
     MOVE     NYU-F05         TO   DNS-F02
     PERFORM  DNSFILF-READ-SEC.
     IF  DNSFILF-INV-FLG  =  "INV"
         DISPLAY NC"＃＃＃　新入出庫ファイルなし" " KEY= "
                  DNS-F01 "," DNS-F02   UPON  CONS
         MOVE    4000             TO    PROGRAM-STATUS
         STOP    RUN
     END-IF.

*新入出庫ファイル更新
     IF   NYU-F06  =  1
          MOVE    NYU-F26         TO   DNS-F18
          ADD     NYU-F18         TO   DNS-F19
          MOVE    NYU-F33         TO   DNS-F20
          MOVE    "1"             TO   DNS-F21
     ELSE
          MOVE    NYU-F26         TO   DNS-F18
          ADD     NYU-F18         TO   DNS-F19
          MOVE    NYU-F33         TO   DNS-F20
          MOVE    SPACE           TO   DNS-F21
     END-IF.
     ADD          1               TO   OUT-CNT.
     REWRITE  DNS-REC.
*発注明細更新
     MOVE         1               TO   HAM-F20.
     REWRITE  HAM-REC.
*入庫ファイル更新
     MOVE         1               TO   NYU-F31.
     MOVE         1               TO   NYU-F92.
     MOVE     WK-SYSDATE          TO   NYU-F93.
     REWRITE  NYU-REC.
*次の入庫ファイルの読込
     PERFORM  NYKFILF-READ-SEC.
*
 200-MAIN-SEC-EXT.
     EXIT.
************************************************************
*    入庫ファイルの読込処理
************************************************************
 NYKFILF-READ-SEC       SECTION.

 NYKFILF-READ-100.
     READ    NYKFILF   NEXT
        AT   END
             MOVE      "END"     TO   END-FLG
             GO        TO        NYKFILF-READ-EXT
     END-READ.

     ADD     1                   TO   IN-CNT.
     IF     IN-CNT-X  =  "500"  OR  "000"
            MOVE   IN-CNT             TO   DSP-READ
            DISPLAY  DSP-READ-AREA  UPON   CONS
     END-IF.

*伝票番号チェック
     IF     NYU-F02  >=  7000000
            CONTINUE
     ELSE
            GO          TO       NYKFILF-READ-100
     END-IF.

*計上フラグ＝１、計上済フラグ＝空白が対象
     IF     NYU-F30  =  1
       AND  NYU-F31  =  ZERO
            CONTINUE
     ELSE
            GO          TO       NYKFILF-READ-100
     END-IF.

     ADD     1                   TO   SEL-CNT.

 NYKFILF-READ-EXT.
     EXIT.
************************************************************
*      発注ファイルの読込
************************************************************
 HACMEIF-READ-SEC       SECTION.
     READ    HACMEIF
       INVALID      KEY
          MOVE      "INV"        TO   HACMEIF-INV-FLG
       NOT INVALID      KEY
          MOVE      SPACE        TO   HACMEIF-INV-FLG
     END-READ.
 HACMEIF-READ-EXT.
     EXIT.
************************************************************
*    新入出庫ファイル読込　　　　　　　　　　
************************************************************
 DNSFILF-READ-SEC       SECTION.
     READ    DNSFILF
       INVALID      KEY
          MOVE      "INV"        TO   DNSFILF-INV-FLG
       NOT INVALID      KEY
          MOVE      SPACE        TO   DNSFILF-INV-FLG
     END-READ.
 DNSFILF-READ-EXT.
     EXIT.
************************************************************
*      ３００     終了処理
************************************************************
 300-END-SEC            SECTION.

     MOVE   IN-CNT               TO   DSP-IN.
     MOVE   OUT-CNT              TO   DSP-OUT.
     DISPLAY  DSP-IN-AREA      UPON   CONS.
     DISPLAY  DSP-OUT-AREA     UPON   CONS.

     CLOSE  NYKFILF
            HACMEIF
            DNSFILF.

 300-END-SEC-EXT.
     EXIT.
*****************<<  PROGRAM  END  >>***********************

```
