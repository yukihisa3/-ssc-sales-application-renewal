# NVT0010B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/NVT0010B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　Ｄ３６５連携　　　　　　　　　　　*
*    モジュール名　　　　：　D365連携データ作成(_卸実績)
*    作成日／作成者　　　：　2020/05/13   NAV                  *
*    処理内容　　　　　　：　_卸ＤＴより連携データを作成
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            NVT0010B.
 AUTHOR.                ASS.II.
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
****<<  _卸原票でーた　  >>******************************
     SELECT   ZTANADT   ASSIGN    TO      DA-01-VI-ZTANADT1
                        ORGANIZATION         IS  INDEXED
                        ACCESS  MODE         IS  SEQUENTIAL
                        RECORD  KEY          IS  TAN-F01
                        FILE STATUS          IS  TAN-STATUS.
****<< サブ商品名称マスタ >>********************************
     SELECT   SUBMEIF   ASSIGN    TO        DA-01-VI-SUBMEIL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   SUB-F011
                                                 SUB-F0121
                                                 SUB-F0122
                                                 SUB-F0123
                        FILE      STATUS    IS   SUB-STATUS.
*倉庫マスタ
     SELECT   ZSOKMS    ASSIGN    TO        DA-01-VI-ZSOKMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       SOK-F01
                        FILE  STATUS   IS   SOK-STATUS.
*
*条件ファイル
     SELECT   HJYOKEN   ASSIGN    TO        DA-01-VI-JYOKEN1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       JYO-F01
                                            JYO-F02
                        FILE  STATUS   IS   JYO-STATUS.
****<< _卸実績データ >>************************************
     SELECT   SNDTANF   ASSIGN    TO        DA-01-S-SNDTANF
                        FILE      STATUS    IS   SND-STATUS.
***************************************************************
 DATA                   DIVISION.
***************************************************************
 FILE                   SECTION.
***************************************************************
****<< _卸原票データ >>*********************************
 FD  ZTANADT.
     COPY     ZTANADT   OF        XFDLIB
              JOINING   TAN       PREFIX.
****<< サブ商品名称マスタ >>*******************************
 FD  SUBMEIF.
     COPY     SUBMEIF   OF        XFDLIB
              JOINING   SUB       PREFIX.
******************************************************************
*    倉庫マスタ
******************************************************************
 FD  ZSOKMS             LABEL RECORD   IS   STANDARD.
     COPY     ZSOKMS    OF        XFDLIB
              JOINING   SOK       PREFIX.
*
******************************************************************
*    条件ファイル
******************************************************************
 FD  HJYOKEN            LABEL RECORD   IS   STANDARD.
     COPY     HJYOKEN   OF        XFDLIB
              JOINING   JYO       PREFIX.
****<< _卸実績データ >>***********************************
 FD  SNDTANF.
     COPY     SNDTANF   OF        XFDLIB
              JOINING   SND       PREFIX.
****  作業領域  ************************************************
 WORKING-STORAGE        SECTION.
****************************************************************
****  ステイタス情報
 01  STATUS-AREA.
     02 TAN-STATUS           PIC  X(02).
     02 SUB-STATUS           PIC  X(02).
     02 SOK-STATUS           PIC  X(02).
     02 JYO-STATUS           PIC  X(02).
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
 01  ZSOKMS-INV-FLG          PIC  X(03)  VALUE  SPACE.
 01  HJYOKEN-INV-FLG         PIC  X(03)  VALUE  SPACE.
****  カウント
 01  CNT-AREA.
     03  IN-CNT              PIC  9(10)  VALUE  ZERO.
     03  IN-CNT-R            REDEFINES  IN-CNT.
         05  FILLER          PIC  X(07).
         05  IN-CNT-X        PIC  X(03).
     03  SEL-CNT             PIC  9(10)  VALUE  ZERO.
     03  OUT-CNT             PIC  9(10)  VALUE  ZERO.
     03  NG1-CNT             PIC  9(10)  VALUE  ZERO.
     03  NG2-CNT             PIC  9(10)  VALUE  ZERO.
****  処理件数表示
 01  DSP-CNT-AREA.
     03  DSP-READ-AREA.
         05  FILLER          PIC  X(23) VALUE
         "*** ZTANADT  (READ) =".
         05  DSP-READ        PIC  ZZZ,ZZ9.
         05  FILLER          PIC  X(15) VALUE
         " * 処理中]]".
     03  DSP-IN-AREA.
         05  FILLER          PIC  X(23) VALUE
         "*** ZTANADT (INPUT) =".
         05  DSP-IN          PIC  ZZZ,ZZ9.
     03  DSP-OUT-AREA.
         05  FILLER          PIC  X(23) VALUE
         "*** ZTANADT(OUTPUT) =".
         05  DSP-OUT         PIC  ZZZ,ZZ9.

**** メッセージ情報           ****
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "NVT0010B".
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
                   PROCEDURE  ZTANADT.
     MOVE   "ZTANADT"         TO    ERR-FL-ID.
     MOVE    TAN-STATUS       TO    ERR-STCD.
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
                   PROCEDURE  ZSOKMS.
     MOVE   "ZSOKMS1 "        TO    ERR-FL-ID.
     MOVE    SOK-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
***
 FILEERR-SEC4           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  HJYOKEN.
     MOVE   "JYOKEN1 "        TO    ERR-FL-ID.
     MOVE    JYO-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC5           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  SNDTANF.
     MOVE   "SNDTANF"         TO    ERR-FL-ID.
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

     OPEN     I-O       ZTANADT.
     OPEN     INPUT     SUBMEIF  ZSOKMS  HJYOKEN.
     OPEN     OUTPUT    SNDTANF.

*入出庫ファイルの読込
     PERFORM  ZTANADT-READ-SEC.

 100-INIT-END.
     EXIT.
************************************************************
*      ２００   主処理
************************************************************
 200-MAIN-SEC           SECTION.

*サブ商品名称マスタ／検索
     MOVE     TAN-F05         TO   SUB-F011.    *> 商品ＣＤ
     MOVE     TAN-F06X        TO   SUB-F012.    *> 品単ＣＤ
     PERFORM  SUBMEIF-READ-SEC.

*_卸実績データ／編集
     MOVE     SPACE               TO   SND-REC.
     INITIALIZE                        SND-REC.
*  作業実績日
     MOVE     TAN-F83(1:4)        TO   SND-F01(1:4).
     MOVE     "/"                 TO   SND-F01(5:1).
     MOVE     TAN-F83(5:2)        TO   SND-F01(6:2).
     MOVE     "/"                 TO   SND-F01(8:1).
     MOVE     TAN-F83(7:2)        TO   SND-F01(9:2).
*  _卸番号
     MOVE     TAN-F01             TO   SND-F02.
*  ＪＡＮＣＤ
     IF  SUBMEIF-INV-FLG  =  SPACE
         MOVE   SUB-D01           TO   SND-F05
     ELSE       *>マスタ無しの場合は、旧商品ＣＤセット
         MOVE   TAN-F05           TO   SND-F04(1:8)
         MOVE   "-"               TO   SND-F04(9:1)
         MOVE   TAN-F06           TO   SND-F04(10:5)
         MOVE   "-"               TO   SND-F04(15:1)
         MOVE   TAN-F08           TO   SND-F04(16:2)
         MOVE   "-"               TO   SND-F04(18:1)
         MOVE   TAN-F09           TO   SND-F04(19:1)
     END-IF.
*  ストックＮｏ
     MOVE     TAN-F84             TO   SND-F06.
*  倉庫ＣＤ、場所ＣＤ
     MOVE     TAN-F04        TO   SOK-F01.
     PERFORM   ZSOKMS-READ-SEC.
     IF   ZSOKMS-INV-FLG  =  "INV"
          MOVE "0"           TO   SND-F07(1:1)
          MOVE  TAN-F04      TO   SND-F07(2:2)
          MOVE  "11"         TO   SND-F11
     ELSE
**********Ｄ３６５倉庫ＣＤ
          MOVE  SOK-F16      TO   SND-F07
**********保管場所取得
          MOVE  42           TO   JYO-F01
          MOVE  SOK-F17      TO   JYO-F02
          PERFORM  HJYOKEN-READ-SEC
          IF  HJYOKEN-INV-FLG = "INV"
              MOVE  "11"     TO   SND-F08
          ELSE
              MOVE   JYO-F14 TO   SND-F08
          END-IF
     END-IF.
* 帳簿在庫数量（符号）
     MOVE     "0"                 TO   SND-F09.
* 帳簿在庫数量
     COMPUTE  SND-F10  =  TAN-F16  *  100.
* 実_在庫数量（符号）
     MOVE     "0"                 TO   SND-F11.
* 自_在庫数量
     COMPUTE  SND-F12  =  TAN-F11  *  100.

*_卸実績データ／出力
     WRITE    SND-REC.
     ADD      1                   TO   OUT-CNT.


*_卸原票Ｆ／更新
     MOVE     "1"                 TO   TAN-F86.
     MOVE     WK-SYSDATE          TO   TAN-F87.
     REWRITE  TAN-REC.

 MAIN-010.
*次の入出庫ファイルの読込
     PERFORM  ZTANADT-READ-SEC.
*
 200-MAIN-SEC-EXT.
     EXIT.
************************************************************
*    _卸実績Ｆ読込　　　　　　
************************************************************
 ZTANADT-READ-SEC       SECTION.

 ZTANADT-READ-100.
     READ    ZTANADT   NEXT
        AT   END
             MOVE      "END"     TO   END-FLG
             GO        TO        ZTANADT-READ-EXT
     END-READ.

     ADD     1                   TO   IN-CNT.
     IF     IN-CNT-X  =  "500"  OR  "000"
            MOVE   IN-CNT             TO   DSP-READ
            DISPLAY  DSP-READ-AREA  UPON   CONS
     END-IF.

*Ｄ３６５計上区分＝空白が対象
     IF     TAN-F86  =  SPACE
            CONTINUE
     ELSE
            ADD    1    TO       NG1-CNT
            GO          TO       ZTANADT-READ-100
     END-IF.
*_卸数＝０は対象外
     IF     TAN-F11  >  ZERO
            CONTINUE
     ELSE
            ADD    1    TO       NG2-CNT
            GO          TO       ZTANADT-READ-100
     END-IF.

     ADD     1                   TO   SEL-CNT.

 ZTANADT-READ-EXT.
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
*      倉庫マスタ読込
************************************************************
 ZSOKMS-READ-SEC        SECTION.
     READ    ZSOKMS
       INVALID      KEY
          MOVE      "INV"        TO   ZSOKMS-INV-FLG
       NOT INVALID      KEY
          MOVE      SPACE        TO   ZSOKMS-INV-FLG
     END-READ.
 ZSOKMS-READ-EXT.
     EXIT.
************************************************************
*      条件ファイル読込　　　　　
************************************************************
 HJYOKEN-READ-SEC       SECTION.
     READ    HJYOKEN
       INVALID      KEY
          MOVE      "INV"        TO   HJYOKEN-INV-FLG
       NOT INVALID      KEY
          MOVE      SPACE        TO   HJYOKEN-INV-FLG
     END-READ.
 HJYOKEN-READ-EXT.
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
     DISPLAY " _卸確定ＤＴ件数= "  IN-CNT    UPON CONS.
     DISPLAY " _卸実績作成件数= "  OUT-CNT   UPON CONS.
     DISPLAY " 確定済件数　　　= "  NG1-CNT   UPON CONS.
     DISPLAY " 数量＝０　件数　= "  NG2-CNT   UPON CONS.
*#### TEST >>>>>>

     CLOSE  ZTANADT
            SUBMEIF  ZSOKMS  HJYOKEN
            SNDTANF.
 300-END-SEC-EXT.
     EXIT.
*****************<<  PROGRAM  END  >>***********************

```
