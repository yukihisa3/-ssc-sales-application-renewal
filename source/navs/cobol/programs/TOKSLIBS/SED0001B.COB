******************************************************************
*
* 　  顧客名           ：(株)サカタのタネ殿
*   　サブシステム名   ：ＨＧ基幹システム　
*   　業務名　　　     ：ＥＤＩＣシステム
*   　モジュール名     ：制御バイト編集処理（共通）３８４
*   　作成日／更新日   ：2015/09/28
*   　作成日／更新者   ：
*   　処理概要         ：ＥＤＩＣ受信ファイルより、制御項目
*                        を排除し、受信編集ファイルに出力す
*                        る。
*
******************************************************************
 IDENTIFICATION         DIVISION.
******************************************************************
 PROGRAM-ID.            SED0001B.
******************************************************************
 AUTHOR.                NAV.
 DATE-WRITTEN.          15/09/28.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
         CONSOLE   IS   CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*ＥＤＩＣ受信ファイル
     SELECT   EDICSND   ASSIGN    TO   DA-01-S-EDICSND
                         ACCESS    MODE SEQUENTIAL
                         FILE STATUS    IS   INF-ST.
*ＥＤＩＣ受信編集ファイル
     SELECT   EDICSNDM   ASSIGN    TO   DA-01-S-EDICSNDM
                         ACCESS    MODE SEQUENTIAL
                         FILE STATUS    IS   OTF-ST.
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*ＥＤＩＣ受信ファイル
******************************************************************
 FD  EDICSND            BLOCK     CONTAINS  1   RECORDS
                        LABEL     RECORD   IS   STANDARD.
 01  SND-REC.
     03  INF-F01                  PIC   X(384).
*
******************************************************************
*ＥＤＩＣ受信編集ファイル
******************************************************************
 FD  EDICSNDM            BLOCK     CONTAINS  1   RECORDS
                        LABEL     RECORD   IS   STANDARD.
     COPY     EDICSNDM  OF        XFDLIB
              JOINING   OTF       PREFIX.
*
******************************************************************
 WORKING-STORAGE        SECTION.
*ワーク項目
 01  END-FLG                      PIC  X(03)     VALUE  SPACE.
 01  RD-CNT                       PIC  9(08)     VALUE  ZERO.
 01  OT-CNT                       PIC  9(08)     VALUE  ZERO.
 01  IX                           PIC  9(04)     VALUE  ZERO.
 01  IY                           PIC  9(04)     VALUE  ZERO.
*プログラムＳＴＡＴＵＳ
 01  WK-ST.
     03  INF-ST                   PIC  X(02).
     03  OTF-ST                   PIC  X(02).
*****  システム日付ワーク
 01  SYSTEM-HIZUKE.
     03  SYSYMD                   PIC  9(06)     VALUE  ZERO.
     03  SYS-DATEW                PIC  9(08)     VALUE  ZERO.
     03  SYS-DATE-R               REDEFINES SYS-DATEW.
         05  SYS-YY               PIC  9(04).
         05  SYS-MM               PIC  9(02).
         05  SYS-DD               PIC  9(02).
*****  システム時刻ワーク
 01  SYS-TIME                     PIC  9(08).
 01  FILLER                       REDEFINES      SYS-TIME.
     03  SYS-HHMMSS               PIC  9(06).
     03  SYS-MS                   PIC  9(02).
***  セクション名
 01  SEC-NAME.
     03  FILLER                   PIC  X(05)     VALUE " *** ".
     03  S-NAME                   PIC  X(30).
*メッセージ出力
 01  FILE-ERR.
     03  INF-ERR                  PIC  N(20)     VALUE
         NC"ＥＤＩＣ受信ファイルエラ－".
     03  OTF-ERR                  PIC  N(20)     VALUE
         NC"ＥＤＩＣ受信編集メッセージエラ－".
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER               PIC  X(05)     VALUE " *** ".
         05  ST-PG                PIC  X(08)     VALUE "SED0001B".
         05  FILLER               PIC  X(11)     VALUE
                                        " START *** ".
     03  MSG-END.
         05  FILLER               PIC  X(05)     VALUE " *** ".
         05  ST-PG                PIC  X(08)     VALUE "SED0001B".
         05  FILLER               PIC  X(11)     VALUE
                                        " END   *** ".
*    日付変換ワーク（パラメタ用）
 01  LINK-AREA.
     03  LINK-IN-KBN              PIC  X(01).
     03  LINK-IN-YMD6             PIC  9(06).
     03  LINK-IN-YMD8             PIC  9(08).
     03  LINK-OUT-RET             PIC  X(01).
     03  LINK-OUT-YMD8            PIC  9(08).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE                  DIVISION.
 DECLARATIVES.
 INF-ERR                    SECTION.
     USE      AFTER         EXCEPTION PROCEDURE  EDICSND.
     DISPLAY       INF-ERR  UPON      CONS.
     DISPLAY       SEC-NAME UPON      CONS.
     DISPLAY       INF-ST   UPON      CONS.
     MOVE          "4000"   TO        PROGRAM-STATUS.
     STOP          RUN.
 OTF-ERR                    SECTION.
     USE      AFTER         EXCEPTION PROCEDURE  EDICSNDM.
     DISPLAY       OTF-ERR  UPON      CONS.
     DISPLAY       SEC-NAME UPON      CONS.
     DISPLAY       OTF-ST   UPON      CONS.
     MOVE          "4000"   TO        PROGRAM-STATUS.
     STOP          RUN.
 END       DECLARATIVES.
******************************************************************
*                                                                *
******************************************************************
 GENERAL-PROCESS       SECTION.
*
     MOVE     "PROCESS-START"      TO   S-NAME.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC
              UNTIL     END-FLG   =    "END".
     PERFORM  END-SEC.
*
******************************************************************
*             初期処理                                         *
******************************************************************
 INIT-SEC               SECTION.
*    DISPLAY  "INIT-SEC"  UPON  CONS.
     MOVE     "INIT-SEC"           TO   S-NAME.
     OPEN     INPUT     EDICSND.
     OPEN     OUTPUT    EDICSNDM.
     DISPLAY  MSG-START UPON CONS.
*システム時刻取得
     ACCEPT   SYS-TIME  FROM      TIME.
*システム日付取得
     ACCEPT   SYSYMD    FROM      DATE.
     MOVE    "3"        TO        LINK-IN-KBN.
     MOVE     SYSYMD    TO        LINK-IN-YMD6.
     CALL    "SKYDTCKB" USING     LINK-IN-KBN
                                  LINK-IN-YMD6
                                  LINK-IN-YMD8
                                  LINK-OUT-RET
                                  LINK-OUT-YMD8.
     IF       LINK-OUT-RET   =    ZERO
              MOVE      LINK-OUT-YMD8  TO   SYS-DATEW
     ELSE
              MOVE      ZERO           TO   SYS-DATEW
     END-IF.
*流通ＢＭＳ受信ファイル読込
     PERFORM  EDICSND-READ-SEC.
*データ無の場合
     IF  END-FLG = "END"
         DISPLAY NC"＃＃処理対象データがありません！！＃＃"
                 UPON CONS
         MOVE  "4010"      TO  PROGRAM-STATUS
         STOP  RUN
     END-IF.
*
 INIT-EXIT.
     EXIT.
******************************************************************
*              メイン処理                                      *
******************************************************************
 MAIN-SEC     SECTION.
*
     MOVE     "MAIN-SEC"          TO   S-NAME.
*受信編集ファイルの初期化
     MOVE      SPACE              TO   OTF-REC.
     INITIALIZE                        OTF-REC.
     MOVE      ZERO               TO   IY.
*
     PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 384
             IF  INF-F01(IX:1) = X"28" OR X"29"
                 CONTINUE
             ELSE
                 ADD  1              TO  IY
                 MOVE INF-F01(IX:1)  TO  OTF-F01(IY:1)
             END-IF
     END-PERFORM.
*レコード出力
     WRITE  OTF-REC.
     ADD       1                   TO   OT-CNT.
*流通ＢＭＳ受信ファイル読込
     PERFORM  EDICSND-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
******************************************************************
*              終了処理                                        *
******************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"           TO   S-NAME.
*ファイルのクローズ
     CLOSE     EDICSND  EDICSNDM.
*
     DISPLAY "RD-CNT = " RD-CNT     UPON CONS.
     DISPLAY "OT-CNT = " OT-CNT     UPON CONS.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
******************************************************************
*            流通ＢＭＳ受信ファイル読込
******************************************************************
 EDICSND-READ-SEC           SECTION.
*
     MOVE    "EDICSND-READ-SEC"           TO   S-NAME.
*
     READ     EDICSND
              AT  END       MOVE  "END"   TO   END-FLG
                            GO     TO     EDICSND-READ-EXIT
              NOT AT  END   ADD    1      TO   RD-CNT
     END-READ.
*
*件数表示
     IF       RD-CNT(6:3)   =  "000"  OR  "500"
              DISPLAY "# READ-CNT = " RD-CNT
              UPON CONS
     END-IF.
*
 EDICSND-READ-EXIT.
     EXIT.
