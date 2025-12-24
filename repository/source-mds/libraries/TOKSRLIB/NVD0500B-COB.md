# NVD0500B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/NVD0500B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　Ｄ３６５連携　　　　　　　　　　　*
*    モジュール名　　　　：　Ｄ３６５連携データ作成（入荷実績）*
*    処理概要　　　　　　：　作業実績ファイルより作業実績データ*
*    　　　　　　　　　　　　を作成する。　　　　　　　　　　　*
*    作成日／作成者　　　：　2019/12/25   ASS.TAKAHASHI        *
*    更新日／更新者　　　：　YYYY/MM/DD                        *
*    　　変更概要　　　　：                                    *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            NVD0500B.
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
****<< 作業実績ファイル >>*********************************
     SELECT   SGYFILF   ASSIGN    TO        DA-01-VI-SGYFILL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   DYNAMIC
                        RECORD    KEY       IS   SGY-F01
                                                 SGY-F02
                        FILE      STATUS    IS   SGY-STATUS.
****<< サブ商品名称マスタ >>********************************
     SELECT   SUBMEIF   ASSIGN    TO        DA-01-VI-SUBMEIL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   SUB-F011
                                                 SUB-F0121
                                                 SUB-F0122
                                                 SUB-F0123
                        FILE      STATUS    IS   SUB-STATUS.
****<< 作業実績データ >>************************************
     SELECT   SNDSGJF   ASSIGN    TO        DA-01-S-SNDSGJF
                        FILE      STATUS    IS   SND-STATUS.
***************************************************************
 DATA                   DIVISION.
***************************************************************
 FILE                   SECTION.
***************************************************************
****<< 作業実績ファイル >>*********************************
 FD  SGYFILF.
*###   COPY   SGYFILF
*###          DISJOINING  XXX  JOINING  SGY  AS PREFIX.
     COPY     SGYFILF   OF        XFDLIB
              JOINING   SGY       PREFIX.
****<< サブ商品名称マスタ >>*******************************
 FD  SUBMEIF.
*###   COPY   SUBMEIF
*###          DISJOINING  XXX  JOINING  SUB  AS PREFIX.
     COPY     SUBMEIF   OF        XFDLIB
              JOINING   SUB       PREFIX.
****<< 作業実績データ >>***********************************
 FD  SNDSGJF.
*###   COPY   SNDSGJF
*###          DISJOINING  XXX  JOINING  SND  AS PREFIX.
     COPY     SNDSGJF   OF        XFDLIB
              JOINING   SND       PREFIX.
****  作業領域  ************************************************
 WORKING-STORAGE        SECTION.
****************************************************************
****  ステイタス情報
 01  STATUS-AREA.
     02 SGY-STATUS           PIC  X(02).
     02 SUB-STATUS           PIC  X(02).
     02 SND-STATUS           PIC  X(02).
****  システム日付・時刻
 01  SYSTEM-HIZUKE.
     03  SYSYMD              PIC  9(06).
     03  WK-SYSDATE          PIC  9(08).
     03  SYSHMS.
         05  WK-SYSTIME      PIC  9(06).
****  フラグ
 01  END-FLG                 PIC  X(03)  VALUE  SPACE.
 01  SUBMEIF-INV-FLG         PIC  X(03)  VALUE  SPACE.
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
         "*** SGYFILL1   (READ) =".
         05  DSP-READ        PIC  Z,ZZZ,ZZZ,ZZ9.
         05  FILLER          PIC  X(15) VALUE
         " * 処理中]]".
     03  DSP-IN-AREA.
         05  FILLER          PIC  X(23) VALUE
         "*** SGYFILL1  (INPUT) =".
         05  DSP-IN          PIC  Z,ZZZ,ZZZ,ZZ9.
     03  DSP-OUT-AREA.
         05  FILLER          PIC  X(23) VALUE
         "*** SNDSGJF  (OUTPUT) =".
         05  DSP-OUT         PIC  Z,ZZZ,ZZZ,ZZ9.

**** メッセージ情報           ****
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "NVD0500B".
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
 LINKAGE                     SECTION.
****************************************************************
 01  PARA-CNT                PIC  9(10).
****************************************************************
 PROCEDURE                   DIVISION
                                USING PARA-CNT.
****************************************************************
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  SGYFILF.
     MOVE   "SGYFILL1"        TO    ERR-FL-ID.
     MOVE    SGY-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC2           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  SUBMEIF.
     MOVE   "SUBMEIL1"        TO    ERR-FL-ID.
     MOVE    SUB-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC3           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  SNDSGJF.
     MOVE   "SNDSGJF"        TO    ERR-FL-ID.
     MOVE    SND-STATUS       TO    ERR-STCD.
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

     OPEN     I-O       SGYFILF.
     OPEN     INPUT     SUBMEIF.
     OPEN     OUTPUT    SNDSGJF.

*作業実績ファイルの読込
     PERFORM  SGYFILF-READ-SEC.

 100-INIT-END.
     EXIT.
************************************************************
*      ２００   主処理
************************************************************
 200-MAIN-SEC           SECTION.

*サブ商品名称マスタ／検索
     MOVE     SGY-F08         TO   SUB-F011.    *> 商品ＣＤ
     MOVE     SGY-F09         TO   SUB-F012.    *> 品単ＣＤ
     PERFORM  SUBMEIF-READ-SEC.

*作業実績データ／編集
     MOVE     SPACE               TO   SND-REC.
     INITIALIZE                        SND-REC.
*  作業実績日
     MOVE     SGY-F05(1:4)        TO   SND-F01(1:4).
     MOVE     "/"                 TO   SND-F01(5:1).
     MOVE     SGY-F05(5:2)        TO   SND-F01(6:2).
     MOVE     "/"                 TO   SND-F01(8:1).
     MOVE     SGY-F05(7:2)        TO   SND-F01(9:2).

     IF  SUBMEIF-INV-FLG  =  SPACE
*  新商品コード
         MOVE   SUB-D02           TO   SND-F02
         MOVE   SPACE             TO   SND-F02
*  旧商品コード
         MOVE   SUB-F011          TO   SND-F03(1:8)
         MOVE   SUB-F012          TO   SND-F03(9:8)
         MOVE   SPACE             TO   SND-F03
*  ＪＡＮＣＤ
         MOVE   SUB-D01           TO   SND-F04
     END-IF.
*  ストックＮｏ
     MOVE     SGY-F07             TO   SND-F05.
*  入庫倉庫コード
     MOVE     "0"                 TO   SND-F06(1:1).
     MOVE     SGY-F04             TO   SND-F06(2:2).
*  入庫場所コード
     MOVE     SPACE               TO   SND-F07.
     MOVE     "00"                TO   SND-F07.
*  数量符号
*****IF  SGY-F06  =  "2"
*        MOVE  "-"                TO   SND-F08
*    ELSE
*        MOVE  "0"                TO   SND-F08
*****END-IF.
     MOVE     "0"                 TO   SND-F08.
*  数量
     COMPUTE  SND-F09  =  SGY-F11  *  100.
     MOVE     SGY-F11             TO   SND-F09.

*作業実績データ／出力
     IF  SGY-F07  NOT =  SPACE
         WRITE    SND-REC
         ADD      1               TO   OUT-CNT
     END-IF.

*作業実績ファイル／更新
     MOVE     1                   TO   SGY-F13.
     MOVE     "1"                 TO   SGY-F90.
     MOVE     WK-SYSDATE          TO   SGY-F91.
     MOVE     WK-SYSTIME          TO   SGY-F92.
     REWRITE  SGY-REC.


*次の作業実績ファイルの読込
     PERFORM  SGYFILF-READ-SEC.
*
 200-MAIN-SEC-EXT.
     EXIT.
************************************************************
*    作業実績ファイルの読込処理
************************************************************
 SGYFILF-READ-SEC       SECTION.

 SGYFILF-READ-100.
     READ    SGYFILF   NEXT
        AT   END
             MOVE      "END"     TO   END-FLG
             GO        TO        SGYFILF-READ-EXT
     END-READ.

     ADD     1                   TO   IN-CNT.
     IF     IN-CNT-X  =  "500"  OR  "000"
            MOVE   IN-CNT             TO   DSP-READ
            DISPLAY  DSP-READ-AREA  UPON   CONS
     END-IF.

*Ｄ３６５計上区分＝空白が対象
     IF     SGY-F90  =  SPACE
            CONTINUE
     ELSE
            GO          TO       SGYFILF-READ-100
     END-IF.

*作業区分＝５８が対象
     IF     SGY-F03  =  "58"
            CONTINUE
     ELSE
            GO          TO       SGYFILF-READ-100
     END-IF.

     ADD     1                   TO   SEL-CNT.

 SGYFILF-READ-EXT.
     EXIT.
************************************************************
*      サブ商品名称マスタの読込
************************************************************
 SUBMEIF-READ-SEC       SECTION.
     READ    SUBMEIF
       INVALID      KEY
          MOVE      "INV"        TO   SUBMEIF-INV-FLG
       NOT INVALID      KEY
          MOVE      SPACE        TO   SUBMEIF-INV-FLG
     END-READ.
 SUBMEIF-READ-EXT.
     EXIT.
************************************************************
*      ３００     終了処理
************************************************************
 300-END-SEC            SECTION.

     MOVE   IN-CNT               TO   DSP-IN.
     MOVE   OUT-CNT              TO   DSP-OUT.
     DISPLAY  DSP-IN-AREA      UPON   CONS.
     DISPLAY  DSP-OUT-AREA     UPON   CONS.

     MOVE   OUT-CNT              TO   PARA-CNT.

*#### TEST <<<<<<
*### DISPLAY "  " UPON CONS.
*### DISPLAY " 件数 ------- "  PARA-CNT  UPON CONS.
*#### TEST >>>>>>

     CLOSE  SGYFILF
            SUBMEIF
            SNDSGJF.
 300-END-SEC-EXT.
     EXIT.
*****************<<  PROGRAM  END  >>***********************

```
