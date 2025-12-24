# SBM0150V

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SBM0150V.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　イオン流通ＢＭＳ　　　　　　　　　*
*    業務名　　　　　　　：　データ抽出処理　　　　　　　　　　*
*    モジュール名　　　　：　受領訂正ＣＳＶ出力                *
*    作成日／更新日　　　：　20112/11/09                       *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　流通ＢＭＳ受領訂正ワークより　　　*
*                            受領訂正ＣＳＶワークファイルを　　*
*                            出力する。                        *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SBM0150V.
 AUTHOR.                NAV.
 DATE-WRITTEN.          12/11/09.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*受領訂正ワーク
     SELECT   BMSJYTW   ASSIGN    TO        DA-01-VI-BMSJYTW1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       JTW-F013  JTW-F04
                                            JTW-F07   JTW-F03
                                            JTW-F09
                        FILE  STATUS   IS   JTW-STS.
*受領訂正ＣＳＶファイル
     SELECT   BMSJYTC   ASSIGN    TO        DA-01-VS-BMSJYTC
                        ORGANIZATION        SEQUENTIAL
                        ACCESS    MODE      SEQUENTIAL
                        FILE  STATUS   IS   JTC-STS.
*
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    受領訂正ワークファイル
******************************************************************
 FD  BMSJYTW            LABEL RECORD   IS   STANDARD.
     COPY     BMSJYTW   OF        XFDLIB
              JOINING   JTW       PREFIX.
******************************************************************
*    受領訂正ＣＳＶファイル
******************************************************************
 FD  BMSJYTC            LABEL RECORD   IS   STANDARD.
     COPY     BMSJYTC   OF        XFDLIB
              JOINING   JTC       PREFIX.
******************************************************************
 WORKING-STORAGE        SECTION.
******************************************************************
*FLG/ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  ZERO.
 01  READ-CNT                PIC  9(07)     VALUE  ZERO.
 01  WRITE-CNT               PIC  9(07)     VALUE  ZERO.
*
*システム日付の編集
 01  SYS-WORKAREA.
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
 01  STS-AREA.
     03  JTW-STS           PIC  X(02).
     03  JTC-STS           PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SBM0150V".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SBM0150V".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SBM0150V".
         05  FILLER         PIC   X(11)  VALUE
                                         " ABEND *** ".
     03  ABEND-FILE.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  AB-FILE        PIC   X(08).
         05  FILLER         PIC   X(06)  VALUE " ST = ".
         05  AB-STS         PIC   X(02).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  SEC-NAME.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(07)  VALUE " SEC = ".
         05  S-NAME         PIC   X(30).
*日付サブルーチン用
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
 LINKAGE                SECTION.
 01  PARA-AREA.
     03  PARA-TOKCD     PIC   9(08).
     03  PARA-HSYUBETU  PIC   X(01).
     03  PARA-HFROM     PIC   9(08).
     03  PARA-HTO       PIC   9(08).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
*PROCEDURE              DIVISION USING PARA-AREA.
 PROCEDURE              DIVISION.
*
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   BMSJYTW.
     MOVE      "BMSJYTW1"   TO   AB-FILE.
     MOVE      JTW-STS      TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   BMSJYTC.
     MOVE      "BMSJYTC"   TO   AB-FILE.
     MOVE      JTC-STS      TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 END     DECLARATIVES.
*****************************************************************
*                                                                *
******************************************************************
 GENERAL-PROCESS       SECTION.
*
     MOVE     "PROCESS-START"     TO   S-NAME.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC   UNTIL  END-FLG = "END".
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
*
     OPEN     INPUT     BMSJYTW
              OUTPUT    BMSJYTC.
*
     DISPLAY  MSG-START UPON CONS.
*
******************
*システム日付編集*
******************
     ACCEPT      SYS-DATE  FROM      DATE.
     MOVE       "3"        TO        LINK-IN-KBN.
     MOVE        SYS-DATE  TO        LINK-IN-YMD6.
     CALL       "SKYDTCKB"   USING   LINK-IN-KBN
                                     LINK-IN-YMD6
                                     LINK-IN-YMD8
                                     LINK-OUT-RET
                                     LINK-OUT-YMD8.
     IF          LINK-OUT-RET   =    ZERO
         MOVE    LINK-OUT-YMD8  TO   SYS-DATEW
     ELSE
         MOVE    ZERO           TO   SYS-DATEW
     END-IF.
*ファイルスタート
     PERFORM  INSTART-SEC.
     IF   END-FLG = "END"
          DISPLAY NC"＃＃　受領訂正ワーク無　＃＃"  UPON CONS
          GO                    TO   INIT-EXIT
     END-IF.
*ファイル読込
     PERFORM INREAD-SEC.
*
 INIT-EXIT.
     EXIT.
*
****************************************************************
*    受領訂正ワークスタート
****************************************************************
 INSTART-SEC                SECTION.
*
     MOVE    "INSTART-SEC"        TO   S-NAME.
*
     MOVE     SPACE               TO   JTW-REC.
     INITIALIZE                        JTW-REC.
*
***  MOVE     PARA-TOKCD          TO   JTW-F011.
     MOVE     ZERO                TO   JTW-F011.
     MOVE     SPACE               TO   JTW-F04.
     MOVE     ZERO                TO   JTW-F07.
     MOVE     SPACE               TO   JTW-F03.
     MOVE     SPACE               TO   JTW-F09.
*
     START  BMSJYTW  KEY  IS  >=  JTW-F013 JTW-F04  JTW-F07
                                  JTW-F03  JTW-F09
            INVALID
            MOVE    "END"         TO   END-FLG
     END-START.
*
 INSTART-EXIT.
     EXIT.
*
****************************************************************
*   受領訂正ワーク読込
****************************************************************
 INREAD-SEC                 SECTION.
*
     MOVE    "INREAD-SEC"   TO   S-NAME.
*
     READ     BMSJYTW  NEXT
         AT  END
              MOVE     "END"      TO   END-FLG
              GO                  TO   INREAD-EXIT
     END-READ.
     ADD     1    TO   READ-CNT.
*
 INREAD-EXIT.
     EXIT.
*
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"          TO   S-NAME.
**  抽出条件判定
*    IF   JTW-F013  >  PARA-TOKCD
*         MOVE   "END"   TO   END-FLG
*             GO  TO   MAIN-EXIT
*    END-IF.
*日付種別
*    IF   PARA-HSYUBETU  =  "1"
*受信日
*      IF   JTW-F011  >=  PARA-HFROM   AND
*           JTW-F011  <=  PARA-HTO
*                GO  TO   MAIN-010
*      END-IF
*    END-IF.
*    IF   PARA-HSYUBETU  =  "2"
*計上日
*      IF  JTW-F07   >=  PARA-HFROM    AND
*          JTW-F07   <=  PARA-HTO
*             GO  TO   MAIN-010
*      END-IF
*    END-IF.
*
***  GO  TO   MAIN-200.
*
*受領訂正ＣＳＶ出力
  MAIN-010.
     MOVE    JTW-REC        TO  JTC-REC.
     WRITE   JTC-REC.
     ADD     1      TO      WRITE-CNT.
*
 MAIN-200.
     PERFORM  INREAD-SEC.
*
 MAIN-EXIT.
     EXIT.
*
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*件数出力
*受領訂正ワーク読込
     DISPLAY "BMSJYTW   READ CNT = " READ-CNT UPON CONS.
*受領訂正ＣＳＶ出力
     DISPLAY "BMSJYTC  WRITE CNT = " WRITE-CNT UPON CONS.
*
     CLOSE     BMSJYTW  BMSJYTC.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
