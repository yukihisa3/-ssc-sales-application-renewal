# SBM0180V

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SBM0180V.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　イオン流通ＢＭＳ対応              *
*    業務名　　　　　　　：　データ出力処理　　　　            *
*    モジュール名　　　　：　返品ＣＳＶ出力　　                *
*    作成日／更新日　　　：　20112/11/12                       *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　流通ＢＭＳ返品ワークより          *
*                            返品ＣＳＶファイルを出力する。    *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SBM0180V.
 AUTHOR.                NAV.
 DATE-WRITTEN.          12/11/12.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*返品ワーク
     SELECT   BMSHEPW   ASSIGN    TO        DA-01-VI-BMSHEPW1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       HPW-F013  HPW-F04
                                            HPW-F011  HPW-F03
                                            HPW-F09
                        FILE  STATUS   IS   HPW-STS.
*返品ＣＳＶファイル
     SELECT   BMSHEPC   ASSIGN    TO        DA-01-VS-BMSHEPC
                        ORGANIZATION        SEQUENTIAL
                        ACCESS    MODE      SEQUENTIAL
                        FILE  STATUS   IS   HPC-STS.
*
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    返品ワークファイル
******************************************************************
 FD  BMSHEPW            LABEL RECORD   IS   STANDARD.
     COPY     BMSHEPW   OF        XFDLIB
              JOINING   HPW       PREFIX.
******************************************************************
*    返品ＣＳＶファイル
******************************************************************
 FD  BMSHEPC            LABEL RECORD   IS   STANDARD.
     COPY     BMSHEPC   OF        XFDLIB
              JOINING   HPC       PREFIX.
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
     03  HPW-STS           PIC  X(02).
     03  HPC-STS           PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SBM0180V".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SBM0180V".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SBM0180V".
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
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION.
*
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   BMSHEPW.
     MOVE      "BMSHEPW1"   TO   AB-FILE.
     MOVE      HPW-STS      TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   BMSHEPC.
     MOVE      "BMSHEPC"   TO   AB-FILE.
     MOVE      HPC-STS      TO   AB-STS.
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
     OPEN     INPUT     BMSHEPW
              OUTPUT    BMSHEPC.
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
          DISPLAY NC"＃＃　返品ワーク無　＃＃"  UPON CONS
          GO                    TO   INIT-EXIT
     END-IF.
*ファイル読込
     PERFORM INREAD-SEC.
*
 INIT-EXIT.
     EXIT.
*
****************************************************************
*    返品ワークスタート
****************************************************************
 INSTART-SEC                SECTION.
*
     MOVE    "INSTART-SEC"        TO   S-NAME.
*
     MOVE     SPACE               TO   HPW-REC.
     INITIALIZE                        HPW-REC.
*
     MOVE     ZERO                TO   HPW-F013.
     MOVE     SPACE               TO   HPW-F04.
     MOVE     ZERO                TO   HPW-F011.
     MOVE     SPACE               TO   HPW-F03.
     MOVE     SPACE               TO   HPW-F09.
*
     START  BMSHEPW  KEY  IS  >=  HPW-F013 HPW-F04  HPW-F011
                                  HPW-F03  HPW-F09
            INVALID
            MOVE    "END"         TO   END-FLG
     END-START.
*
 INSTART-EXIT.
     EXIT.
*
****************************************************************
*   返品ワーク読込
****************************************************************
 INREAD-SEC                 SECTION.
*
     MOVE    "INREAD-SEC"   TO   S-NAME.
*
     READ     BMSHEPW  NEXT
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
*
*受領訂正ＣＳＶ出力
  MAIN-010.
     MOVE    HPW-REC        TO  HPC-REC.
     WRITE   HPC-REC.
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
*返品ワーク読込
     DISPLAY "BMSHEPW   READ CNT = " READ-CNT UPON CONS.
*返品ＣＳＶ出力
     DISPLAY "BMSHEPC  WRITE CNT = " WRITE-CNT UPON CONS.
*
     CLOSE     BMSHEPW  BMSHEPC.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
