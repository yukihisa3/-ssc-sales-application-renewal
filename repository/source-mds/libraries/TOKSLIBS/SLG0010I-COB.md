# SLG0010I

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SLG0010I.COB`

## ソースコード

```cobol
*************************************************************
* 顧客名　　　　: _サカタのタネ殿
* 業務名　　　　: 内部統制対応
* サブシステム名: ＷＳログ採取管理対応
* モジュール名　: 端末ログ出力情報取得
* 作成日        : 08/08/18
* 作成者　　    : NAV   高橋
* 処理概要　　　: ＷＳログ端末情報を取得しＷＳログ情報出力ＣＬ
*                 を起動する
* 使用ＭＥＤ　　: FWSLOG
*-----------------------------------------------------------*
* 修正日／修正者:
* 修正内容      :
*************************************************************
 IDENTIFICATION      DIVISION.
*************************************************************
 PROGRAM-ID.         SLG0010I.
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
*----<< 画面ファイル       >>----------------*
     SELECT   DSPF
                          ASSIGN      TO        21-GS-DSPF
                          ORGANIZATION          SEQUENTIAL
                          ACCESS MODE           SEQUENTIAL
                          SYMBOLIC DESTINATION  "DSP"
                          PROCESSING MODE       DSP-PROC
                          GROUP                 DSP-GROUP
                          FORMAT                DSP-FORMAT
                          SELECTED FUNCTION     DSP-FUNC
                          FILE STATUS           DSP-STS.
*----<< ＷＳ端末マスタ     >>----------------*
     SELECT   WSIDMST     ASSIGN      TO        01-VI-WSIDMSL1
                          ORGANIZATION          INDEXED
                          ACCESS MODE           SEQUENTIAL
                          RECORD KEY            WSID-NAME
                          FILE STATUS           WSID-STS.
*=============================================================
 DATA                DIVISION.
*=============================================================
 FILE                SECTION.
*----<< 画面ファイル       >>----------------*
 FD  DSPF
                     LABEL  RECORD    IS   STANDARD.
     COPY   FWSLOG    OF        XMDLIB.
*----<< ＷＳ端末マスタ      >>----------------*
 FD  WSIDMST
                     LABEL  RECORD    IS   STANDARD.
     COPY   WSIDMST   OF        XFDLIB
            JOINING   WSID      PREFIX.
*=============================================================
 WORKING-STORAGE     SECTION.
*=============================================================
*画面制御用領域
 01  DSP-CONTROL.
     03  DSP-PROC                PIC  X(02).
     03  DSP-GROUP               PIC  X(08).
     03  DSP-FORMAT              PIC  X(08).
     03  DSP-FUNC                PIC  X(04).
*ステータス領域
 01  WORK-AREA.
   03  STATUS-AREA.
       05  DSP-STS               PIC  X(02).
       05  WSID-STS              PIC  X(02).
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
 01  OUTAREA            PIC  X(06)     VALUE  SPACE.
*プログラムスイッチ（画面遷移制御）
 01  PSW-AREA.
     03  PSW                     PIC  X(01).
 01  MSG-AREA.
     03  ERR01                   PIC  N(30)      VALUE
         NC"日付がエラー".
     03  ERR02                   PIC  N(30)      VALUE
         NC"システム日付を入力日付が超えています".
     03  GID01                   PIC  N(30)      VALUE
     NC"ＰＦ９＝取消ＰＦ１２＝戻りＰＦ１６＝終了".
*    ﾒﾂｾｰｼﾞ
 01  MSG-AREA.
     03  MSG-ABEND1              PIC X(22)   VALUE
                                 "### SLG0010I ABEND ###".
     03  MSG-ABEND2.
         05  FILLER              PIC X(04)   VALUE   "### ".
         05  ERR-FL-ID           PIC X(08).
         05  FILLER              PIC X(04)   VALUE   " ST-".
         05  ERR-STCD            PIC X(02).
         05  FILLER              PIC X(04)   VALUE   " ###".
 LINKAGE                SECTION.
 01  PARA-KUBUN              PIC X(01).
*=============================================================
 PROCEDURE           DIVISION    USING   PARA-KUBUN.
*=============================================================
**
 DECLARATIVES.
 FILEERR-SEC1            SECTION.
     USE AFTER   EXCEPTION
                 PROCEDURE       DSPF.
     MOVE    "DSPF    "          TO      ERR-FL-ID.
     MOVE    DSP-STS             TO      ERR-STCD.
     DISPLAY MSG-ABEND1          UPON    CONS.
     DISPLAY MSG-ABEND2          UPON    CONS.
     STOP     RUN.
**
 FILEERR-SEC2            SECTION.
     USE AFTER   EXCEPTION
                 PROCEDURE       WSIDMST.
     MOVE    "WSIDMST "          TO      ERR-FL-ID.
     MOVE    WSID-STS            TO      ERR-STCD.
     DISPLAY MSG-ABEND1          UPON    CONS.
     DISPLAY MSG-ABEND2          UPON    CONS.
     STOP     RUN.
**
**
 END DECLARATIVES.
**************************************************************
*      < 0.0 >     PROGRAM-START
**************************************************************
 PROGRAM-START        SECTION.
     PERFORM   INIT-SEC.
     PERFORM   MAIN-SEC      UNTIL     END-FLG  =  "END".
     PERFORM   FAIN-SEC.
 PROGRAM-START-EXIT.
     STOP      RUN.
**************************************************************
*      < 1.0 >     初期処理
**************************************************************
 INIT-SEC            SECTION.
**** OPEN     I-O         DSPF.
     OPEN     INPUT       WSIDMST.
*フラグの初期化
     INITIALIZE           WORK-AREA.
*初期画面の表示
**** PERFORM  INIT-DSP-SUB.
**2008/05/19 STA
     ACCEPT   SYS-DATE          FROM  DATE.
     DISPLAY "PARA-KUBUN = " PARA-KUBUN UPON CONS.
     IF       PARA-KUBUN  =  SPACE
              OPEN     I-O         DSPF
              PERFORM  INIT-DSP-SUB
*ヘッド入力へ
              MOVE    "1"          TO  PSW
     ELSE
              MOVE      SYS-DATE          TO  DSPYMD
*ログ発行
              MOVE    "3"          TO  PSW
     END-IF.
**2008/05/19 END
 INIT-END.
     EXIT.
**************************************************************
*      < 1.1 >      初期画面表示
**************************************************************
 INIT-DSP-SUB       SECTION.
*画面の初期化
     MOVE      SPACE             TO  FWSLOG.
*
***  ACCEPT    SYS-DATE          FROM  DATE.
     MOVE      SYS-DATE          TO  DSPYMD.
     MOVE      "CL"              TO  DSP-PROC.
     PERFORM   DSP-WRITE-SUB.
     MOVE      SPACE             TO  DSP-PROC.
 INIT-DSP-EXIT.
     EXIT.
**************************************************************
*      2.0       メイン処理
**************************************************************
 MAIN-SEC            SECTION.
     MOVE    SPACE          TO  DSPMSG.
     EVALUATE  PSW
         WHEN  "1"     PERFORM  DSP-HIZ-SUB
**       DISPLAY "AAA" UPON CONS
         WHEN  "2"     PERFORM  DSP-KKN-SUB
**       DISPLAY "BBB" UPON CONS
         WHEN  "3"     PERFORM  LOGLIST-OUT-SEC
                                UNTIL END-FLG = "END"
**       DISPLAY "CCC" UPON CONS
         WHEN  OTHER   CONTINUE
     END-EVALUATE.
 MAIN-END.
     EXIT.
**************************************************************
*      2.1  日付入力処理
**************************************************************
 DSP-HIZ-SUB         SECTION.
*
     PERFORM   DSP-READ-SUB.
     EVALUATE  DSP-FUNC
         WHEN "E000"
             MOVE    DSPYMD(1:2)     TO  W-DSPSKY
             MOVE    DSPYMD(3:2)     TO  W-DSPSKM
             IF  W-DSPSKY            >   90
                 MOVE    19          TO  W-DSPSKY-4
             ELSE
                 MOVE    20          TO  W-DSPSKY-4
             END-IF
             MOVE    SPACE           TO  FLG-YMD
             MOVE    DSPYMD          TO  W-DSPYMD
             PERFORM     YMD-CHEK-SUB
             MOVE    W-DSPENDD       TO  W-DSPSKD
             IF  FLG-YMD         NOT =   SPACE
                 MOVE    ERR01       TO  DSPMSG
             ELSE
                 MOVE    "2"         TO  PSW
                 IF  SYS-DATE   =   DSPYMD
                     MOVE "@ACTWS"   TO  OUTAREA
                 ELSE
                     IF  SYS-DATE  >  DSPYMD
                         MOVE "@TOP"   TO  OUTAREA
                     ELSE
                         MOVE    "ERR"       TO  FLG-YMD
                         MOVE    ERR02       TO  DSPMSG
                         MOVE    "1"         TO  PSW
                     END-IF
                 END-IF
             END-IF
         WHEN "F009"
             MOVE    SPACE           TO  FWSLOG
             MOVE    "1"             TO  PSW
         WHEN "F012"
             MOVE    "1"             TO  PSW
         WHEN "F016"
             MOVE    "END"           TO  END-FLG
         WHEN    OTHER
             CONTINUE
     END-EVALUATE.
     PERFORM   DSP-WRITE-SUB.
 DSP-HIZ-EXIT.
     EXIT.
**************************************************************
*      2.1  確認入力処理
**************************************************************
 DSP-KKN-SUB         SECTION.
*
**2008/05/23 STR
     PERFORM   DSP-READ-SUB.
**2008/05/23 END
     EVALUATE  DSP-FUNC
         WHEN "E000"
             MOVE    "3"             TO  PSW
         WHEN "F009"
             MOVE    SPACE           TO  FWSLOG
             MOVE    "1"             TO  PSW
         WHEN "F012"
             MOVE    "1"             TO  PSW
         WHEN "F016"
             MOVE    "END"           TO  END-FLG
         WHEN    OTHER
             CONTINUE
     END-EVALUATE.
**2008/05/23 STR
     PERFORM   DSP-WRITE-SUB.
**2008/05/23 END
 DSP-KKN-EXIT.
     EXIT.
**************************************************************
*      2.1  日付チェック処理
**************************************************************
 YMD-CHEK-SUB         SECTION.
*
     EVALUATE  W-DSPM
         WHEN 01
         WHEN 03
         WHEN 05
         WHEN 07
         WHEN 08
         WHEN 10
         WHEN 12
             IF  W-DSPD                <   1
             OR  W-DSPD                >   31
                 MOVE    "ERR"         TO  FLG-YMD
             END-IF
             MOVE    31                TO  W-DSPENDD
         WHEN 02
             COMPUTE     W-DSPY
                                       =   W-DSPY
                                       -   (W-DSPY  /  4  *  4)
             IF  W-DSPY                =   0
                 IF  W-DSPD            <   1
                 OR  W-DSPD            >   29
                     MOVE    "ERR"     TO  FLG-YMD
                 END-IF
                 MOVE    29            TO  W-DSPENDD
             ELSE
                 IF  W-DSPD            <   1
                 OR  W-DSPD            >   28
                     MOVE    "ERR"     TO  FLG-YMD
                 END-IF
                 MOVE    28            TO  W-DSPENDD
             END-IF
         WHEN 04
         WHEN 06
         WHEN 09
         WHEN 11
             IF  W-DSPD                <   1
             OR  W-DSPD                >   30
                 MOVE    "ERR"         TO  FLG-YMD
             END-IF
             MOVE    30                TO  W-DSPENDD
         WHEN    OTHER
             MOVE    "ERR"             TO  FLG-YMD
     END-EVALUATE.
 YMD-CHEK-EXIT.
     EXIT.
**************************************************************
*      < 0.0 >     端末ログ出力処理
**************************************************************
 LOGLIST-OUT-SEC    SECTION.

     PERFORM     WSIDMST-READ-SEC.

     IF   END-FLG  NOT  =  "END"
          CALL     "PLG00200"  USING   WSID-NAME
                                       OUTAREA
                                       DSPYMD
     END-IF.
*
 LOGLIST-OUT-EXIT.
     EXIT.
**************************************************************
*                 ＷＳ端末マスタ読込み
**************************************************************
 WSIDMST-READ-SEC         SECTION.
     READ    WSIDMST
         AT  END
             MOVE    "END"     TO  END-FLG
             MOVE    "9"       TO  PSW
         NOT AT END
             ADD      1        TO  READ-CNT
     END-READ.
 WSIDMST-REDT-EXIT.
     EXIT.
**************************************************************
*      2.X       画面表示処理
**************************************************************
 DSP-WRITE-SUB       SECTION.
*画面の表示
     MOVE    GID01                 TO  DSPGID.
     MOVE    "SCREEN"              TO  DSP-GROUP.
     MOVE    "FWSLOG"              TO  DSP-FORMAT.
     WRITE   FWSLOG.
 DSP-WRITE-END.
     EXIT.
**************************************************************
*      2.Y       画面読込処理
**************************************************************
 DSP-READ-SUB       SECTION.
     MOVE    "NE"                  TO  DSP-PROC.
     EVALUATE    PSW
         WHEN    "1"
             MOVE    "GRPHIZ"      TO  DSP-GROUP
         WHEN    "2"
             MOVE    "GRPKKN"      TO  DSP-GROUP
         WHEN    OTHER
             CONTINUE
     END-EVALUATE.
*入力項目の属性を通常にする
     MOVE    "FWSLOG"          TO  DSP-FORMAT.
     READ    DSPF.
     MOVE    SPACE             TO  DSP-PROC.
 DSP-READ-END.
     EXIT.
**************************************************************
*      3.0        終了処理
**************************************************************
 FAIN-SEC              SECTION.
*
**2008/05/23 STA
**** CLOSE           DSPF   WSIDMST.
     IF       PARA-KUBUN  =  SPACE
              CLOSE           DSPF   WSIDMST
     END-IF.
**2008/05/23 END
*
     IF   READ-CNT   =  ZERO
          DISPLAY  NC"＃＃対象データがありません＃＃"  UPON CONS
     END-IF.
 FAIN-EXIT.
*****************<<  SLG0010I END PROGRAM  >>*******************

```
