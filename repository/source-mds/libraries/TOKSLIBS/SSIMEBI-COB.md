# SSIMEBI

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSIMEBI.COB`

## ソースコード

```cobol
******************************************************************
*
* 　  顧客名             ： (株)サカタのタネ殿
*   　サブシステム名     ： ＨＧ基幹システム　
*   　業務名　　　       ： 流通ＢＭＳ
*   　モジュール名       ： 請求締日保管（コメリ）
*   　作成日／更新日     ： 2013/06/28
*   　作成日／更新者     ： ナブ井上
*   　処理概要           ： 条件ファイルＫＥＹ：５９の
*                           請求締日を、ＫＥＹ：７９ヘ
*                           保管する。
*                           当締日は請求ＭＳＧ作成時に
*                           セットする値をして使用される。
*
******************************************************************
 IDENTIFICATION         DIVISION.
******************************************************************
 PROGRAM-ID.            SSIMEBI.
******************************************************************
 AUTHOR.                NAV.
 DATE-WRITTEN.          13/06/28.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
         CONSOLE   IS   CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*
*条件ファイル
     SELECT   HJYOKEN   ASSIGN         DA-01-VI-JYOKEN1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  JYO-F01
                                       JYO-F02
                        FILE STATUS    IS   JYO-ST.
*
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*条件ファイル
******************************************************************
 FD  HJYOKEN
                        LABEL     RECORD   IS   STANDARD.
     COPY     HJYOKEN   OF        XFDLIB
              JOINING   JYO       PREFIX.
*
******************************************************************
 WORKING-STORAGE        SECTION.
*ワーク項目
 01  END-FLG                      PIC  X(03)     VALUE  SPACE.
 01  INV-FLG                      PIC  X(03)     VALUE  SPACE.
 01  WK-MSG-DATA                  PIC  X(05)     VALUE  SPACE.
*
*プログラムＳＴＡＴＵＳ
 01  WK-ST.
     03  JYO-ST                   PIC  X(02).
*
*バッチ
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
     03  JYO-ERR                  PIC  N(20)     VALUE
         NC"条件ファイルエラー　　　　　　".
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER               PIC  X(05)     VALUE " *** ".
         05  ST-PG                PIC  X(08)     VALUE "SSIMEBI ".
         05  FILLER               PIC  X(11)     VALUE
                                        " START *** ".
     03  MSG-END.
         05  FILLER               PIC  X(05)     VALUE " *** ".
         05  ST-PG                PIC  X(08)     VALUE "SSIMEBI ".
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
 01  SIME-DATE          PIC  9(08).
 01  FILLER             REDEFINES      SIME-DATE.
     03  SIME-YY        PIC  9(04).
     03  SIME-MM        PIC  9(02).
     03  SIME-DD        PIC  9(02).
 01  SSKTLSTD-DATE      PIC  9(08).
 01  FILLER             REDEFINES      SSKTLSTD-DATE.
     03  SSKTLSTD-YY    PIC  9(04).
     03  SSKTLSTD-MM    PIC  9(02).
     03  SSKTLSTD-DD    PIC  9(02).
 01  SSKTLSTD-RET       PIC  9(01).
*
*パラメタ定義
 LINKAGE                SECTION.
* 入力パラメタ
*    区分　１：２０日締　２：末日締
 01  PARA-IN-KBN                  PIC  X(01).
* 出力パラメタ
 01  PARA-OUT-SIMEBI              PIC  9(08).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE DIVISION USING   PARA-IN-KBN
                            PARA-OUT-SIMEBI.
*
 DECLARATIVES.
 JYO-ERR                    SECTION.
     USE      AFTER         EXCEPTION PROCEDURE  HJYOKEN.
     DISPLAY       JYO-ERR  UPON      CONS.
     DISPLAY       SEC-NAME UPON      CONS.
     DISPLAY       JYO-ST   UPON      CONS.
     MOVE          "4000"   TO        PROGRAM-STATUS.
     STOP          RUN.
 END       DECLARATIVES.
******************************************************************
*                                                                *
******************************************************************
 GENERAL-PROCESS       SECTION.
*
*    DISPLAY "GENERAL-PROCESS" UPON CONS.
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
     OPEN     I-O       HJYOKEN.
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
*
     MOVE     SPACE     TO        END-FLG.
*
*条件ファイル読込 （ＫＥＹ：５９）
     PERFORM  JYO59-READ-SEC.
 INIT-EXIT.
     EXIT.
******************************************************************
*              メイン処理                                      *
******************************************************************
 MAIN-SEC     SECTION.
*
*    DISPLAY "MAIN-SEC"  UPON  CONS.
     MOVE    "MAIN-SEC"          TO   S-NAME.
*
 MAIN-01.
*条件ファイル読込 （ＫＥＹ：７９）
     PERFORM  JYO79-READ-SEC.
     IF       INV-FLG  =  "INV"
              GO    TO    MAIN-02
     ELSE
              GO    TO    MAIN-03
     END-IF.
*
 MAIN-02.
*条件ファイル登録 （ＫＥＹ：７９）
     PERFORM  JYO79-WRITE-SEC.
     MOVE    "END"  TO END-FLG.
     GO       TO    MAIN-EXIT.
*
 MAIN-03.
*条件ファイル更新 （ＫＥＹ：７９）
     PERFORM  JYO79-REWRITE-SEC.
     MOVE    "END"  TO END-FLG.
*
 MAIN-EXIT.
     EXIT.
******************************************************************
*              終了処理                                        *
******************************************************************
 END-SEC       SECTION.
*
*    DISPLAY "END-SEC" UPON  CONS.
     MOVE     "END-SEC"           TO   S-NAME.
*ファイルのクローズ
     CLOSE     HJYOKEN.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*
******************************************************************
*            条件ファイル読込（ＫＥＹ：５９）
******************************************************************
 JYO59-READ-SEC           SECTION.
*
     MOVE    "JYO59-READ-SEC"             TO   S-NAME.
*
     MOVE     59                          TO   JYO-F01.
     MOVE    " "                          TO   JYO-F02.
     READ     HJYOKEN
              INVALID
                      DISPLAY NC"条件ファイルなし！"   UPON CONS
                      DISPLAY NC"　ＫＥＹ１＝" "59"    UPON CONS
                      DISPLAY NC"　ＫＥＹ２＝" " "     UPON CONS
                      MOVE  "END"   TO   END-FLG
                      MOVE  "4000"  TO   PROGRAM-STATUS
              NOT INVALID
                      MOVE  JYO-F04 TO   SSKTLSTD-DATE
                                         SIME-DATE
     END-READ.
*
 JYO59-READ-EXIT.
     EXIT.
*
******************************************************************
*            条件ファイル読込（ＫＥＹ：７９）
******************************************************************
 JYO79-READ-SEC           SECTION.
*
     MOVE    "JYO79-READ-SEC"             TO   S-NAME.
*
     MOVE     79                          TO   JYO-F01.
     MOVE     PARA-IN-KBN                 TO   JYO-F02.
     READ     HJYOKEN
              INVALID
                      DISPLAY NC"条件ファイルなし！"   UPON CONS
                      DISPLAY NC"　ＫＥＹ１＝" "79"    UPON CONS
                      DISPLAY NC"　ＫＥＹ２＝" PARA-IN-KBN
                                                       UPON CONS
                      DISPLAY NC"　→作成します"       UPON CONS
                      MOVE  "INV"   TO   INV-FLG
              NOT INVALID
                      MOVE  "   "   TO   INV-FLG
     END-READ.
*
 JYO79-READ-EXIT.
     EXIT.
*
******************************************************************
*            条件ファイル登録（ＫＥＹ：７９）
******************************************************************
 JYO79-WRITE-SEC           SECTION.
*
     MOVE    "JYO79-WRITE-SEC"            TO   S-NAME.
     INITIALIZE  JYO-REC.
     MOVE     79                          TO   JYO-F01.
     MOVE     PARA-IN-KBN                 TO   JYO-F02.
*
     IF       SIME-DD   =    31
              CALL "OSKTLSTD"     USING   SSKTLSTD-DATE
                                          SSKTLSTD-RET
          IF  SSKTLSTD-RET  =  9
              DISPLAY  NC"月末日計算処理異常！"      UPON CONS
              DISPLAY  NC"　算出元日付＝" SIME-DATE  UPON CONS
              MOVE  "END"   TO   END-FLG
              MOVE  "4000"  TO   PROGRAM-STATUS
              GO            TO   JYO79-REWRITE-EXIT
          END-IF
     END-IF.
*
     MOVE     SSKTLSTD-DATE               TO   PARA-OUT-SIMEBI
                                               JYO-F04.
     WRITE    JYO-REC.
*
 JYO79-WRITE-EXIT.
     EXIT.
*
******************************************************************
*            条件ファイル更新（ＫＥＹ：７９）
******************************************************************
 JYO79-REWRITE-SEC           SECTION.
*
     MOVE    "JYO79-REWRITE-SEC"          TO   S-NAME.
*
     IF       SIME-DD   =    31
              CALL "OSKTLSTD"     USING   SSKTLSTD-DATE
                                          SSKTLSTD-RET
          IF  SSKTLSTD-RET  =  9
              DISPLAY  NC"月末日計算処理異常！"      UPON CONS
              DISPLAY  NC"　算出元日付＝" SIME-DATE  UPON CONS
              MOVE  "END"   TO   END-FLG
              MOVE  "4000"  TO   PROGRAM-STATUS
              GO            TO   JYO79-REWRITE-EXIT
          END-IF
     END-IF.
*
     MOVE     SSKTLSTD-DATE               TO   PARA-OUT-SIMEBI
                                               JYO-F04.
     REWRITE  JYO-REC.
*
 JYO79-REWRITE-EXIT.
     EXIT.
*

```
