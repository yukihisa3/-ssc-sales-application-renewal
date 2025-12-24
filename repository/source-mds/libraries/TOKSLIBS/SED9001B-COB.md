# SED9001B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SED9001B.COB`

## ソースコード

```cobol
******************************************************************
* 　  顧客名             ： (株)サカタのタネ殿
*   　サブシステム名     ： ＨＧ基幹システム　
*   　業務名　　　       ： ＥＤＩＣシステム（共通）
*   　モジュール名       ： ＥＤＩＣ名称取得（サブルーチン）
*   　作成日／更新日     ： 2015/08/19
*   　作成日／更新者     ：
*   　処理概要           ： パラメタを受取、ＥＤＩＣ名称
*                           マスタを読み、名称を取得しパ
*                           ラメタＯＵＴする。
*   パラメタ説明
*     (IN ) 9(08) 取引先コード
*     (IN ) X(06) ＥＤＩＣ名称区分コード
*     (IN ) X(10) ＥＤＩＣ名称コード
*     (OUT) N(20) ＥＤＩＣ名称名
*     (OUT) X(20) ＥＤＩＣ名称名カナ
*     (OUT) X(01) 結果（空白：正常、空白以外：異常）
*
*     更新日／更新者     ：
*     更新日／更新者     ：
*     修正概要           ：
******************************************************************
 IDENTIFICATION         DIVISION.
******************************************************************
 PROGRAM-ID.            SED9001B.
******************************************************************
 AUTHOR.                NAV.
 DATE-WRITTEN.          15/08/19.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
         CONSOLE   IS   CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*ＥＤＩＣ名称マスタ
     SELECT   EDMEISF   ASSIGN         DA-01-VI-EDMEISL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  MEI-F01
                                       MEI-F02
                                       MEI-F04
                        FILE STATUS    IS   MEI-ST.
*
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*ＥＤＩＣ名称マスタ
******************************************************************
 FD  EDMEISF
                        LABEL     RECORD   IS   STANDARD.
     COPY     EDMEISF   OF        XFDLIB
              JOINING   MEI       PREFIX.
*
******************************************************************
 WORKING-STORAGE        SECTION.
*ワーク項目
 01  END-FLG                      PIC  X(03)     VALUE  SPACE.
 01  EDMEISF-INV-FLG              PIC  X(03)     VALUE  SPACE.
*プログラムＳＴＡＴＵＳ
 01  WK-ST.
     03  MEI-ST                   PIC  X(02).
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
     03  MEI-ERR                  PIC  N(20)     VALUE
         NC"ＥＤＩＣ名称マスタエラー".
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER               PIC  X(05)     VALUE " *** ".
         05  ST-PG                PIC  X(08)     VALUE "SED9001B".
         05  FILLER               PIC  X(11)     VALUE
                                        " START *** ".
     03  MSG-END.
         05  FILLER               PIC  X(05)     VALUE " *** ".
         05  ST-PG                PIC  X(08)     VALUE "SED9001B".
         05  FILLER               PIC  X(11)     VALUE
                                        " END   *** ".
*    日付変換ワーク（パラメタ用）
 01  LINK-AREA.
     03  LINK-IN-KBN              PIC  X(01).
     03  LINK-IN-YMD6             PIC  9(06).
     03  LINK-IN-YMD8             PIC  9(08).
     03  LINK-OUT-RET             PIC  X(01).
     03  LINK-OUT-YMD8            PIC  9(08).
*パラメタ定義
 LINKAGE                SECTION.
* 入力パラメタ
 01  PARA-IN-TOKCD                PIC  9(08).
 01  PARA-IN-MEIKBN               PIC  X(06).
 01  PARA-IN-MEICD                PIC  X(10).
 01  PARA-OUT-MEISYO1             PIC  N(20).
 01  PARA-OUT-MEISYO2             PIC  X(20).
 01  PARA-OUT-KEKA                PIC  X(01).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE DIVISION USING   PARA-IN-TOKCD
                            PARA-IN-MEIKBN
                            PARA-IN-MEICD
                            PARA-OUT-MEISYO1
                            PARA-OUT-MEISYO2
                            PARA-OUT-KEKA.
 DECLARATIVES.
 MEI-ERR                    SECTION.
     USE      AFTER         EXCEPTION PROCEDURE  EDMEISF.
     DISPLAY       MEI-ERR  UPON      CONS.
     DISPLAY       SEC-NAME UPON      CONS.
     DISPLAY       MEI-ST   UPON      CONS.
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
     PERFORM  MAIN-SEC.
     PERFORM  END-SEC.
*
******************************************************************
*             初期処理                                         *
******************************************************************
 INIT-SEC               SECTION.
*
     MOVE     "INIT-SEC"           TO   S-NAME.
*
     OPEN     INPUT     EDMEISF.
*
*    DISPLAY "PARA-IN-TOKCD  = " PARA-IN-TOKCD  UPON CONS.
*    DISPLAY "PARA-IN-MEIKBN = " PARA-IN-MEIKBN UPON CONS.
*    DISPLAY "PARA-IN-MEICD  = " PARA-IN-MEICD  UPON CONS.
*
 INIT-EXIT.
     EXIT.
******************************************************************
*              メイン処理                                      *
******************************************************************
 MAIN-SEC     SECTION.
*
     MOVE     "MAIN-SEC"          TO   S-NAME.
*ＥＤＩＣ名称を取得する
     MOVE     PARA-IN-TOKCD            TO  MEI-F01.
     MOVE     PARA-IN-MEIKBN           TO  MEI-F02.
     MOVE     PARA-IN-MEICD            TO  MEI-F04.
     PERFORM  EDMEISF-READ-SEC.
     IF  EDMEISF-INV-FLG = SPACE
         MOVE MEI-F05                  TO  PARA-OUT-MEISYO1
         MOVE MEI-F06                  TO  PARA-OUT-MEISYO2
         MOVE SPACE                    TO  PARA-OUT-KEKA
         GO                            TO  MAIN-EXIT
     END-IF.
*指定されたキー情報が存在しない場合
*取引先コードをＺＥＲＯ（代表）で索引
     MOVE     ZERO                     TO  MEI-F01.
     MOVE     PARA-IN-MEIKBN           TO  MEI-F02.
     MOVE     PARA-IN-MEICD            TO  MEI-F04.
     PERFORM  EDMEISF-READ-SEC.
     IF  EDMEISF-INV-FLG = SPACE
         MOVE MEI-F05                  TO  PARA-OUT-MEISYO1
         MOVE MEI-F06                  TO  PARA-OUT-MEISYO2
         MOVE SPACE                    TO  PARA-OUT-KEKA
         GO                            TO  MAIN-EXIT
     END-IF.
*
     MOVE     SPACE                    TO  PARA-OUT-MEISYO1.
     MOVE     SPACE                    TO  PARA-OUT-MEISYO2.
     MOVE     "1"                      TO  PARA-OUT-KEKA.
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
     CLOSE     EDMEISF.
*
*    DISPLAY "PARA-OUT-MEISYO1 = " PARA-OUT-MEISYO1 UPON CONS.
*    DISPLAY "PARA-OUT-MEISYO2 = " PARA-OUT-MEISYO2 UPON CONS.
*    DISPLAY "PARA-KEKA        = " PARA-OUT-KEKA    UPON CONS.
*
 END-EXIT.
     EXIT     PROGRAM.
******************************************************************
*ＥＤＩＣ名称マスタ読込
******************************************************************
 EDMEISF-READ-SEC      SECTION.
     MOVE  "EDMEISF-READ-SEC"             TO   S-NAME.
*読込
     READ  EDMEISF
           INVALID     MOVE "INV"         TO   EDMEISF-INV-FLG
           NOT INVALID MOVE SPACE         TO   EDMEISF-INV-FLG
     END-READ.
*
 EDMEISF-READ-EXIT.
     EXIT.

```
