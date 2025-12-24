# SSY5705B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSY5705B.COB`

## ソースコード

```cobol
******************************************************************
* 　  顧客名         ： (株)サカタのタネ殿
*   　サブシステム名 ： ＨＧ基幹システム　
*   　業務名　　　   ： グッデイウタネ連携廃止
*   　モジュール名   ： 受信データ制御バイト削除
*   　作成日／更新日 ： 2014/06/16
*   　作成日／更新者 ：
*   　処理概要       ： グッデイ受信データを読み、制御項目を
*                       を排除し、受信編集ファイルに出力する。
*                       出力する。
******************************************************************
 IDENTIFICATION         DIVISION.
******************************************************************
 PROGRAM-ID.            SSY5705B.
******************************************************************
 AUTHOR.                NAV.
 DATE-WRITTEN.          14/06/16.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
         CONSOLE   IS   CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*グッデイ発注受信ファイル
     SELECT   HACGODY   ASSIGN    TO   DA-01-S-HACGODYN
                        ACCESS    MODE SEQUENTIAL
                        FILE STATUS    IS   RCV-ST.
*グッデイ発注受信編集ファイル
     SELECT   HACGODYW  ASSIGN    TO   DA-01-S-HACGODYW
                        ACCESS    MODE SEQUENTIAL
                        FILE STATUS    IS   RWK-ST.
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*グッデイ発注受信ファイル
******************************************************************
 FD  HACGODY            BLOCK     CONTAINS  1   RECORDS
                        LABEL     RECORD   IS   STANDARD.
 01  RCV-REC.
     03  RCV-F01        PIC  X(256).
******************************************************************
*グッデイ発注受信編集ファイル
******************************************************************
 FD  HACGODYW           BLOCK     CONTAINS  1   RECORDS
                        LABEL     RECORD   IS   STANDARD.
 01  RWK-REC.
     03  RWK-F01        PIC  X(256).
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
     03  RCV-ST                   PIC  X(02).
     03  RWK-ST                   PIC  X(02).
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
     03  RCV-ERR                  PIC  N(20)     VALUE
         NC"グッデイ発注受信ファイルエラ－".
     03  RWK-ERR                  PIC  N(20)     VALUE
         NC"グッデイ発注受信編集Ｆエラー".
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER               PIC  X(05)     VALUE " *** ".
         05  ST-PG                PIC  X(08)     VALUE "SSY5705B".
         05  FILLER               PIC  X(11)     VALUE
                                        " START *** ".
     03  MSG-END.
         05  FILLER               PIC  X(05)     VALUE " *** ".
         05  ST-PG                PIC  X(08)     VALUE "SSY5705B".
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
 RCV-ERR                    SECTION.
     USE      AFTER         EXCEPTION PROCEDURE  HACGODY.
     DISPLAY       RCV-ERR  UPON      CONS.
     DISPLAY       SEC-NAME UPON      CONS.
     DISPLAY       RCV-ST   UPON      CONS.
     MOVE          "4000"   TO        PROGRAM-STATUS.
     STOP          RUN.
 RWK-ERR                    SECTION.
     USE      AFTER         EXCEPTION PROCEDURE  HACGODYW.
     DISPLAY       RWK-ERR  UPON      CONS.
     DISPLAY       SEC-NAME UPON      CONS.
     DISPLAY       RWK-ST   UPON      CONS.
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
     OPEN     INPUT     HACGODY.
     OPEN     OUTPUT    HACGODYW.
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
*グッデイ発注受信ファイル読込
     PERFORM  HACGODY-READ-SEC.
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
     MOVE      SPACE              TO   RWK-REC.
     INITIALIZE                        RWK-REC.
     MOVE      ZERO               TO   IY.
*
     PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 256
             IF  RCV-F01(IX:1) = X"28" OR X"29"
                 CONTINUE
             ELSE
                 ADD  1              TO  IY
                 MOVE RCV-F01(IX:1)  TO  RWK-F01(IY:1)
             END-IF
     END-PERFORM.
*レコード出力
     WRITE  RWK-REC.
     ADD       1                   TO   OT-CNT.
*グッデイ発注受信ファイル読込
     PERFORM  HACGODY-READ-SEC.
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
     CLOSE     HACGODY  HACGODYW.
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
 HACGODY-READ-SEC           SECTION.
*
     MOVE    "HACGODY-READ-SEC"           TO   S-NAME.
*
     READ     HACGODY
              AT  END       MOVE  "END"   TO   END-FLG
                            GO     TO     HACGODY-READ-EXIT
              NOT AT  END   ADD    1      TO   RD-CNT
     END-READ.
*
*件数表示
     IF       RD-CNT(6:3)   =  "000"  OR  "500"
              DISPLAY "# READ-CNT = " RD-CNT
              UPON CONS
     END-IF.
*
 HACGODY-READ-EXIT.
     EXIT.

```
