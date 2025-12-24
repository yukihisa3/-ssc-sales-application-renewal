# SLG0030B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SLG0030B.COB`

## ソースコード

```cobol
*************************************************************
* 顧客名　　　　: _サカタのタネ殿
* 業務名　　　　: 内部統制対応
* サブシステム名: ＷＳログ採取管理対応
* モジュール名　: スプール情報制御　　　
* 作成日        : 08/08/25
* 作成者　　    : NAV   高橋
* 処理概要　　　: ＷＳログ端末情報を取得しＷＳログ情報出力ＣＬ
*                 を起動する
* 使用ＭＥＤ　　:
*-----------------------------------------------------------*
* 修正日／修正者:
* 修正内容      :
*************************************************************
 IDENTIFICATION      DIVISION.
*************************************************************
 PROGRAM-ID.         SLG0030B.
 DATE-WRITTEN.       08.04.23.
 ENVIRONMENT         DIVISION.
 CONFIGURATION       SECTION.
 SOURCE-COMPUTER.    FACOM-K.
 OBJECT-COMPUTER.    FACOM-K.
 SPECIAL-NAMES.
     CONSOLE      IS     CONS
     STATION      IS     STA.
*
 INPUT-OUTPUT        SECTION.
 FILE-CONTROL.
*----<< ＷＳ端末マスタ     >>----------------*
     SELECT   SPOOLPF     ASSIGN      TO        01-VI-SPOOLL1
                          ORGANIZATION          INDEXED
                          ACCESS MODE           SEQUENTIAL
                          RECORD KEY            SPL-F01
                                                SPL-F02
                                                SPL-F03
                          FILE STATUS           SPL-STS.
*=============================================================
 DATA                DIVISION.
*=============================================================
 FILE                SECTION.
*----<< ＷＳログマスタ      >>----------------*
 FD  SPOOLPF
                     LABEL  RECORD    IS   STANDARD.
     COPY   SPOOLPF   OF        XFDLIB
            JOINING   SPL       PREFIX.
*=============================================================
 WORKING-STORAGE     SECTION.
*=============================================================
*ステータス領域
 01  WORK-AREA.
   03  STATUS-AREA.
       05  SPL-STS               PIC  X(02).
*フラグ
   03  FLG-AREA.
       05  END-FLG               PIC  X(03).
       05  FLG-DSP               PIC  X(03).
       05  FLG-CAL               PIC  X(03).
       05  FLG-TEN               PIC  X(03).
       05  FLG-ALL               PIC  X(03).
       05  FLG-CHK               PIC  X(03).
       05  FLG-YMD               PIC  X(03).
   03  IX.
       05  I1                    PIC  9(03).
       05  I2                    PIC  9(03).
       05  I3                    PIC  9(03).
   03  SYS-YMD.
       05  SYS-YM.
         07  SYS-Y               PIC  9(02).
         07  SYS-M               PIC  9(02).
       05  SYS-D                 PIC  9(02).
   03  W-DSPSKYMD-4.
       05  W-DSPSKYM-4.
           07  W-DSPSKY-4      PIC  9(02).
           07  W-DSPSKYM.
               09  W-DSPSKY    PIC  9(02).
               09  W-DSPSKM    PIC  9(02).
       05  W-DSPSKD            PIC  9(02).
   03  W-DSPYMD.
       05  W-DSPY              PIC  9(02).
       05  W-DSPM              PIC  9(02).
       05  W-DSPD              PIC  9(02).
   03  W-DSPENDD                 PIC  9(02).
   03  YB-YMD.
       05  YB-YM.
           07  YB-YY           PIC  9(04).
           07  YB-MM           PIC  9(02).
       05  YB-DD               PIC  9(02).
   03  YB-YOBI                 PIC  9(01).
   03  YB-WEEK                 PIC  9(01).
   03  W-YOB-TBL.
       05  W-YOB-DAT             OCCURS  31.
         07  W-WEEK              PIC  9(01).
         07  W-YOB               PIC  N(01).
 01  SYS-DATE                    PIC  9(06).
 01  FILLER             REDEFINES      SYS-DATE.
     03  SYS-YY         PIC  9(02).
     03  SYS-MM         PIC  9(02).
     03  SYS-DD         PIC  9(02).
 01  CNT-AREA.
     03  READ-CNT       PIC  9(03)     VALUE  ZERO.
*    ﾒﾂｾｰｼﾞ
 01  MSG-AREA.
     03  MSG-ABEND1              PIC X(22)   VALUE
                                 "### SLG0030B ABEND ###".
     03  MSG-ABEND2.
         05  FILLER              PIC X(04)   VALUE   "### ".
         05  ERR-FL-ID           PIC X(08).
         05  FILLER              PIC X(04)   VALUE   " ST-".
         05  ERR-STCD            PIC X(02).
         05  FILLER              PIC X(04)   VALUE   " ###".
*=============================================================
 PROCEDURE           DIVISION.
*=============================================================
**
 DECLARATIVES.
 FILEERR-SEC1            SECTION.
     USE AFTER   EXCEPTION
                 PROCEDURE       SPOOLPF.
     MOVE    "SPOOLL1 "          TO      ERR-FL-ID.
     MOVE    SPL-STS             TO      ERR-STCD.
     DISPLAY MSG-ABEND1          UPON    CONS.
     DISPLAY MSG-ABEND2          UPON    CONS.
     STOP     RUN.
**
 END DECLARATIVES.
**************************************************************
*      < 0.0 >     PROGRAM-START
**************************************************************
 PROGRAM-START        SECTION.
*
     PERFORM   INIT-SEC.
     PERFORM   MAIN-SEC      UNTIL     END-FLG  =  "END".
     PERFORM   FAIN-SEC.
*
 PROGRAM-START-EXIT.
     STOP      RUN.
**************************************************************
*      < 1.0 >     初期処理
**************************************************************
 INIT-SEC            SECTION.
     OPEN     INPUT       SPOOLPF.
*フラグの初期化
     INITIALIZE           WORK-AREA.
*ｽﾌﾟｰﾙ情報ﾌｧｲﾙ読込み
     PERFORM  SPOOLPF-READ-SEC.
*
 INIT-END.
     EXIT.
**************************************************************
*      2.0       メイン処理
**************************************************************
 MAIN-SEC            SECTION.
*ｽﾌﾟｰﾙ情報出力
      CALL     "PKYSPLQB"  USING   SPL-F02  SPL-F03.
*ｽﾌﾟｰﾙ情報ﾌｧｲﾙ読込み
     PERFORM  SPOOLPF-READ-SEC.
*
 MAIN-END.
     EXIT.
**************************************************************
*      3.0        終了処理
**************************************************************
 FAIN-SEC              SECTION.
*
     CLOSE  SPOOLPF.
*
     DISPLAY "SPLF COPY CNT = " READ-CNT UPON CONS.
*
 FAIN-EXIT.
**************************************************************
*                 スプールファイル読込
**************************************************************
 SPOOLPF-READ-SEC          SECTION.
*
     READ    SPOOLPF
         AT  END
             MOVE    "END"     TO  END-FLG
             GO                TO  SPOOLPF-READ-EXIT
         NOT AT END
             ADD      1        TO  READ-CNT
     END-READ.
*
 SPOOLPF-READ-EXIT.
     EXIT.
*****************<<  SLG0030B END PROGRAM  >>*******************

```
