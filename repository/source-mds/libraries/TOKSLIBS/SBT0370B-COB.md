# SBT0370B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SBT0370B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ＨＧ基幹システム　　　　　　　　　*
*    業務名　　　　　　　：　ＬＩＮＫＳ連携　　　　　　　　　　*
*    モジュール名　　　　：　出荷連携データ累積（カインズ）　  *
*    作成日／更新日　　　：　2014/07/31                        *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　先行処理にて抽出された、　　　　  *
*                            出荷連携データ（カインズ）を　　　*
*                            累積ファイルに累積する。          *
*　　更新日／更新者　　　：　                                  *
*    修正概要　　　　　　：                                    *
*      　　　　　　　　　　　　　　　　　　　　　　　　　　　　*
*      　　　　　　　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SBT0370B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2014/07/31.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*カインズ出荷連携データワーク
     SELECT   CNZSYWF   ASSIGN    TO        DA-01-S-CNZSYWF
                        ORGANIZATION        SEQUENTIAL
                        ACCESS    MODE      SEQUENTIAL
                        FILE      STATUS    IS   SYW-ST.
*カインズ出荷連携累積データ
     SELECT   CNZSYRF   ASSIGN    TO        DA-01-S-CNZSYRF
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   SYR-ST.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    カインズ出荷連携データワーク
******************************************************************
 FD  CNZSYWF            BLOCK CONTAINS 1    RECORDS
                        LABEL RECORD   IS   STANDARD.
     COPY     CNZSYWF   OF        XFDLIB
              JOINING   SYW  AS   PREFIX.
*
******************************************************************
*    カインズ出荷連携累積データ
******************************************************************
 FD  CNZSYRF            LABEL     RECORD     IS  STANDARD.
     COPY     CNZSYRF   OF        XFDLIB
              JOINING   SYR       PREFIX.
******************************************************************
 WORKING-STORAGE        SECTION.
*ワーク項目
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  WRT-CNT1                PIC  9(08)     VALUE  ZERO.
 01  WRT-CNT2                PIC  9(08)     VALUE  ZERO.
 01  WK-SYW-F112             PIC  9(08)     VALUE  ZERO.
 01  HMEIMS-INV-FLG          PIC  X(03)     VALUE  SPACE.
 01  HSHOTBL-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  HIDUKE-HENKAN           PIC  9(08)     VALUE  ZERO.
 01  WK-SYW-F15              PIC  9(10)     VALUE  ZERO.
 01  WK-SYW-F50              PIC  9(10)     VALUE  ZERO.
 01  TANE-SIZAI              PIC  X(01)     VALUE  SPACE.
*プログラムＳＴＡＴＵＳ.
 01  WK-ST.
     03  SYW-ST        PIC  X(02).
     03  SYR-ST        PIC  X(02).
***** システム日付ワーク
 01  SYSTEM-HIZUKE.
     03  SYSYMD              PIC   9(06)  VALUE  ZERO.
     03  SYS-DATEW           PIC   9(08)  VALUE  ZERO.
     03  SYS-DATE-R          REDEFINES SYS-DATEW.
         05  SYS-YY          PIC   9(04).
         05  SYS-MM          PIC   9(02).
         05  SYS-DD          PIC   9(02).
***** システム時刻ワーク
 01  SYS-TIME           PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME.
     03  SYS-HHMNSS     PIC  9(06).
     03  SYS-MS         PIC  9(02).

***  セクション名
 01  SEC-NAME.
     03  FILLER             PIC  X(05)     VALUE " *** ".
     03  S-NAME             PIC  X(30).
*メッセージ出力
 01  FILE-ERR.
     03  SYW-ERR           PIC  N(20)  VALUE
         NC"カインズ出荷連携データワークエラー".
     03  SYR-ERR           PIC  N(20)  VALUE
         NC"カインズ出荷連携累積データエラー".
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SBT0370B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SBT0370B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-IN.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(10)  VALUE " INPUT  = ".
         05  IN-CNT         PIC   9(08).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  MSG-OUT.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(10)  VALUE " OUTPUT = ".
         05  OUT-CNT        PIC   9(08).
         05  FILLER         PIC   X(05)  VALUE " *** ".
*    日付変換ワーク（パラメタ用）
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*パラメタ定義
*LINKAGE                SECTION.
* 入力パラメタ
*    倉庫CD
*01  PARA-SOKOCD        PIC   X(02).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE DIVISION.
 DECLARATIVES.
 SYW-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE CNZSYWF.
     DISPLAY     SYW-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     SYW-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 SYR-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE CNZSYRF.
     DISPLAY     SYR-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     SYR-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
*
 END     DECLARATIVES.
*****************************************************************
*                                                                *
******************************************************************
 GENERAL-PROCESS       SECTION.
*
     MOVE     "PROCESS-START"     TO   S-NAME.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC
              UNTIL     END-FLG    =   "END".
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     INPUT     CNZSYWF.
     OPEN     EXTEND    CNZSYRF.
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
              MOVE    ZERO             TO   SYS-DATEW
     END-IF.
*
     MOVE     SPACE     TO        END-FLG.
     MOVE     ZERO      TO        RD-CNT    WRT-CNT1.
     MOVE     ZERO      TO        IN-CNT    OUT-CNT.
*カインズ出荷連携データワーク読込
     PERFORM  CNZSYWF-READ-SEC.
     IF       END-FLG = "END"
              GO             TO   INIT-EXIT
     END-IF.
*
 INIT-010.
*
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*レコード初期化
     MOVE     SPACE               TO   SYR-REC.
     INITIALIZE                        SYR-REC.
*レコード
     MOVE   SYW-REC               TO   SYR-REC.
     WRITE  SYR-REC.
*対象件数確認
     ADD      1                   TO   WRT-CNT1.
*売上伝票ファイル読込
     PERFORM  CNZSYWF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*プログラム終了メッセージ表示
     MOVE      RD-CNT    TO      IN-CNT.
     MOVE      WRT-CNT1  TO      OUT-CNT.
     DISPLAY   MSG-IN    UPON CONS.
     DISPLAY   MSG-OUT   UPON CONS.
     DISPLAY   MSG-END   UPON CONS.
*ファイルクローズ
     CLOSE     CNZSYWF   CNZSYRF.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
****************************************************************
*　　　　　　カインズ出荷連携データワーク読込
****************************************************************
 CNZSYWF-READ-SEC          SECTION.
*
     READ     CNZSYWF
              AT  END       MOVE  "END"   TO  END-FLG
                            GO TO CNZSYWF-READ-EXIT
              NOT AT  END   ADD    1      TO  RD-CNT
     END-READ.
*件数表示
     IF       RD-CNT(6:3)  =  "000" OR "500"
              DISPLAY "# READ-CNT = " RD-CNT "(" WRT-CNT1 ") #"
                      UPON CONS
     END-IF.
*
 CNZSYWF-READ-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
