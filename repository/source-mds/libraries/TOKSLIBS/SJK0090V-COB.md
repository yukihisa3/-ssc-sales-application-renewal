# SJK0090V

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SJK0090V.COB`

## ソースコード

```cobol
******************************************************************
*     顧客名　　　　　　 ： サカタのタネ殿
*     サブシステム名　　 ： 受注自動欠品機能
*     業務名　　　　　　 ： 受注自動欠品機能
*     モジュール名　　　 ： 欠品リストＣＳＶ出力
*     作成日／更新日　　 ： 2015/10/16
*     作成日／更新者　　 ：
*     処理概要　　　　　 ： 欠品ＣＳＶデータを読み、ＣＳＶ
*                           データを出力する。
*                           データを出力する。
*     更新日／更新者　　 ：
*     更新日／更新者　　 ：
*　　 修正概要　　　　　 ：
*
******************************************************************
 IDENTIFICATION         DIVISION.
******************************************************************
 PROGRAM-ID.            SJK0090V.
******************************************************************
 AUTHOR.                NAV.
 DATE-WRITTEN.          15/10/16.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
         CONSOLE   IS   CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*欠品ＣＳＶデータ
     SELECT   CSVKEPF      ASSIGN    TO  DA-01-VI-CSVKEPL1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      SEQUENTIAL
                           RECORD    KEY       KEP-F01
                                               KEP-F05
                                               KEP-F09
                                               KEP-F03
                                               KEP-F04
                           FILE      STATUS    KEP-ST.
*欠品ＣＳＶワーク
     SELECT   CSVKEPWK     ASSIGN    TO  DA-01-S-CSVKEPWK
                           FILE      STATUS    EWK-ST.
*
******************************************************************
 DATA                   DIVISION.
 FILE                   SECTION.
*欠品ＣＳＶデータ
 FD  CSVKEPF.
     COPY        CSVKEPF   OF        XFDLIB
     JOINING     KEP       AS        PREFIX.
*
*欠品ＣＳＶワーク
 FD  CSVKEPWK           BLOCK        CONTAINS  1    RECORDS.
*
 01 EWK-REC.
    03  FILLER                    PIC  X(1000).
******************************************************************
 WORKING-STORAGE        SECTION.
*
 01  WK-KOUMOKU.
     03  FILLER PIC  X(01)  VALUE X"28".
     03  FILLER PIC  N(06)  VALUE NC"取引先コード".
     03  FILLER PIC  X(01)  VALUE X"29".
     03  FILLER PIC  X(01)  VALUE ",".
     03  FILLER PIC  X(01)  VALUE X"28".
     03  FILLER PIC  N(04)  VALUE NC"取引先名".
     03  FILLER PIC  X(01)  VALUE X"29".
     03  FILLER PIC  X(01)  VALUE ",".
     03  FILLER PIC  X(01)  VALUE X"28".
     03  FILLER PIC  N(04)  VALUE NC"伝票番号".
     03  FILLER PIC  X(01)  VALUE X"29".
     03  FILLER PIC  X(01)  VALUE ",".
     03  FILLER PIC  X(01)  VALUE X"28".
     03  FILLER PIC  N(03)  VALUE NC"行番号".
     03  FILLER PIC  X(01)  VALUE X"29".
     03  FILLER PIC  X(01)  VALUE ",".
     03  FILLER PIC  X(01)  VALUE X"28".
     03  FILLER PIC  N(05)  VALUE NC"店舗コード".
     03  FILLER PIC  X(01)  VALUE X"29".
     03  FILLER PIC  X(01)  VALUE ",".
     03  FILLER PIC  X(01)  VALUE X"28".
     03  FILLER PIC  N(03)  VALUE NC"店舗名".
     03  FILLER PIC  X(01)  VALUE X"29".
     03  FILLER PIC  X(01)  VALUE ",".
     03  FILLER PIC  X(01)  VALUE X"28".
     03  FILLER PIC  N(04)  VALUE NC"出荷場所".
     03  FILLER PIC  X(01)  VALUE X"29".
     03  FILLER PIC  X(01)  VALUE ",".
     03  FILLER PIC  X(01)  VALUE X"28".
     03  FILLER PIC  N(05)  VALUE NC"出荷場所名".
     03  FILLER PIC  X(01)  VALUE X"29".
     03  FILLER PIC  X(01)  VALUE ",".
     03  FILLER PIC  X(01)  VALUE X"28".
     03  FILLER PIC  N(03)  VALUE NC"納品日".
     03  FILLER PIC  X(01)  VALUE X"29".
     03  FILLER PIC  X(01)  VALUE ",".
     03  FILLER PIC  X(01)  VALUE X"28".
     03  FILLER PIC  N(07)  VALUE NC"相手商品コード".
     03  FILLER PIC  X(01)  VALUE X"29".
     03  FILLER PIC  X(01)  VALUE ",".
     03  FILLER PIC  X(01)  VALUE X"28".
     03  FILLER PIC  N(08)  VALUE NC"サカタ商品コード".
     03  FILLER PIC  X(01)  VALUE X"29".
     03  FILLER PIC  X(01)  VALUE ",".
     03  FILLER PIC  X(01)  VALUE X"28".
     03  FILLER PIC  N(06)  VALUE NC"サカタ品単１".
     03  FILLER PIC  X(01)  VALUE X"29".
     03  FILLER PIC  X(01)  VALUE ",".
     03  FILLER PIC  X(01)  VALUE X"28".
     03  FILLER PIC  N(06)  VALUE NC"サカタ品単２".
     03  FILLER PIC  X(01)  VALUE X"29".
     03  FILLER PIC  X(01)  VALUE ",".
     03  FILLER PIC  X(01)  VALUE X"28".
     03  FILLER PIC  N(06)  VALUE NC"サカタ品単３".
     03  FILLER PIC  X(01)  VALUE X"29".
     03  FILLER PIC  X(01)  VALUE ",".
     03  FILLER PIC  X(01)  VALUE X"28".
     03  FILLER PIC  N(06)  VALUE NC"商品名カナ１".
     03  FILLER PIC  X(01)  VALUE X"29".
     03  FILLER PIC  X(01)  VALUE ",".
     03  FILLER PIC  X(01)  VALUE X"28".
     03  FILLER PIC  N(06)  VALUE NC"商品名カナ２".
     03  FILLER PIC  X(01)  VALUE X"29".
     03  FILLER PIC  X(01)  VALUE ",".
     03  FILLER PIC  X(01)  VALUE X"28".
     03  FILLER PIC  N(04)  VALUE NC"受注数量".
     03  FILLER PIC  X(01)  VALUE X"29".
     03  FILLER PIC  X(01)  VALUE ",".
     03  FILLER PIC  X(01)  VALUE X"28".
     03  FILLER PIC  N(04)  VALUE NC"出荷数量".
     03  FILLER PIC  X(01)  VALUE X"29".
     03  FILLER PIC  X(01)  VALUE ",".
     03  FILLER PIC  X(01)  VALUE X"28".
     03  FILLER PIC  N(04)  VALUE NC"欠品数量".
     03  FILLER PIC  X(01)  VALUE X"29".
     03  FILLER PIC  X(01)  VALUE ",".
     03  FILLER PIC  X(01)  VALUE X"28".
     03  FILLER PIC  N(07)  VALUE NC"自動欠品ＦＬＧ".
     03  FILLER PIC  X(01)  VALUE X"29".
*
 01  WK-MEISAI.
     03  MEI-F01            PIC    9(08).
     03  MEI-A01            PIC    X(01).
     03  MEI-F021           PIC    X(01).
     03  MEI-F02            PIC    N(15).
     03  MEI-F022           PIC    X(01).
     03  MEI-A02            PIC    X(01).
     03  MEI-F03            PIC    9(09).
     03  MEI-A03            PIC    X(01).
     03  MEI-F04            PIC    9(02).
     03  MEI-A04            PIC    X(01).
     03  MEI-F05            PIC    9(05).
     03  MEI-A05            PIC    X(01).
     03  MEI-F061           PIC    X(01).
     03  MEI-F06            PIC    N(15).
     03  MEI-F062           PIC    X(01).
     03  MEI-A06            PIC    X(01).
     03  MEI-F07            PIC    X(02).
     03  MEI-A07            PIC    X(01).
     03  MEI-F081           PIC    X(01).
     03  MEI-F08            PIC    N(15).
     03  MEI-F082           PIC    X(01).
     03  MEI-A08            PIC    X(01).
     03  MEI-F09            PIC    X(10).
     03  MEI-A09            PIC    X(01).
     03  MEI-F10            PIC    X(13).
     03  MEI-A10            PIC    X(01).
     03  MEI-F11            PIC    X(08).
     03  MEI-A11            PIC    X(01).
     03  MEI-F12            PIC    X(05).
     03  MEI-A12            PIC    X(01).
     03  MEI-F13            PIC    X(02).
     03  MEI-A13            PIC    X(01).
     03  MEI-F14            PIC    X(01).
     03  MEI-A14            PIC    X(01).
     03  MEI-F15            PIC    X(15).
     03  MEI-A15            PIC    X(01).
     03  MEI-F16            PIC    X(15).
     03  MEI-A16            PIC    X(01).
     03  MEI-F17            PIC    9(07).
     03  MEI-A17            PIC    X(01).
     03  MEI-F18            PIC    9(07).
     03  MEI-A18            PIC    X(01).
     03  MEI-F19            PIC    9(07).
     03  MEI-A19            PIC    X(01).
     03  MEI-F20            PIC    X(01).

*ワーク項目
 01  END-FLG                      PIC  X(03)     VALUE  SPACE.
 01  RD-CNT                       PIC  9(08)     VALUE  ZERO.
 01  WRT-CNT                      PIC  9(08)     VALUE  ZERO.
*小数点編集
 01  WK-HENSYU1.
     03  WK-11-KETA               PIC  9(09)V9(02).
     03  WK-11-KETA-R             REDEFINES      WK-11-KETA.
         05  WK-9-KETA            PIC  9(09).
         05  WK-2-KETA            PIC  9(02).
 01  WK-HENSYU2.
     03  WK-7-KETA                PIC  9(06)V9(01).
     03  WK-7-KETA-R              REDEFINES      WK-7-KETA.
         05  WK-6-KETA            PIC  9(06).
         05  WK-1-KETA            PIC  9(01).
*プログラムＳＴＡＴＵＳ
 01  WK-ST.
     03  KEP-ST                   PIC  X(02).
     03  EWK-ST                   PIC  X(02).
*日付編集
 01  WK-NOUHIN-BI.
     03  WK-NOUHIN-YYYY           PIC  X(04).
     03  WK-KUGIRI1               PIC  X(01).
     03  WK-NOUHIN-MM             PIC  X(02).
     03  WK-KUGIRI2               PIC  X(01).
     03  WK-NOUHIN-DD             PIC  X(02).
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
     03  KEP-ERR                  PIC  N(20)     VALUE
         NC"欠品ＣＳＶデータエラ－".
     03  EWK-ERR                  PIC  N(20)     VALUE
         NC"欠品ＣＳＶワークエラ－".
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER               PIC  X(05)     VALUE " *** ".
         05  ST-PG                PIC  X(08)     VALUE "SJK0090V".
         05  FILLER               PIC  X(11)     VALUE
                                        " START *** ".
     03  MSG-END.
         05  FILLER               PIC  X(05)     VALUE " *** ".
         05  ST-PG                PIC  X(08)     VALUE "SJK0090V".
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
 PROCEDURE DIVISION.
 DECLARATIVES.
 HEP-ERR                    SECTION.
     USE      AFTER         EXCEPTION PROCEDURE  CSVKEPF.
     DISPLAY       KEP-ERR  UPON      CONS.
     DISPLAY       SEC-NAME UPON      CONS.
     DISPLAY       KEP-ST   UPON      CONS.
     MOVE          "4000"   TO        PROGRAM-STATUS.
     STOP          RUN.
 HWK-ERR                    SECTION.
     USE      AFTER         EXCEPTION PROCEDURE  CSVKEPWK.
     DISPLAY       EWK-ERR  UPON      CONS.
     DISPLAY       SEC-NAME UPON      CONS.
     DISPLAY       EWK-ST   UPON      CONS.
     MOVE          "4000"   TO        PROGRAM-STATUS.
     STOP          RUN.
 END       DECLARATIVES.
******************************************************************
*                                                                *
******************************************************************
 CONTROL-SUB           SECTION.
*
     MOVE     "CONTROL-SUB"       TO    S-NAME.
*
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC
              UNTIL     END-FLG   =    "END".
     PERFORM  END-SEC.
 CONTROL-EXIT.
     EXIT.
*
******************************************************************
*             初期処理　　　　　　　　　　　　　　　　　　　　 *
******************************************************************
 INIT-SEC               SECTION.
*
     MOVE    "INIT-SEC"           TO    S-NAME.
*
     OPEN     INPUT     CSVKEPF.
     OPEN     OUTPUT    CSVKEPWK.
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
     MOVE     SPACE               TO        END-FLG.
     MOVE     ZERO                TO        RD-CNT
                                            WRT-CNT.
*欠品ＣＳＶデータ読込
     PERFORM  CSVKEPF-READ-SEC.
*終了チェック（存在した場合は、タイトル行出力）
     IF  END-FLG  NOT =  "END"
         MOVE   SPACE             TO        EWK-REC
*        INITIALIZE                         EWK-REC
         MOVE   WK-KOUMOKU        TO        EWK-REC
         WRITE  EWK-REC
         ADD    1                 TO        WRT-CNT
     END-IF.
*
 INIT-EXIT.
     EXIT.
******************************************************************
*              メイン処理　　　　　　　　　　　　　　　　　　　*
******************************************************************
 MAIN-SEC     SECTION.
*
     MOVE     "MAIN-SEC"          TO   S-NAME.
*ワーク初期化後後転送
     MOVE      SPACE              TO   WK-MEISAI.
     INITIALIZE                        WK-MEISAI.
*ファイル内容を転送
     MOVE      KEP-F01            TO   MEI-F01.
     MOVE      KEP-F02            TO   MEI-F02.
     MOVE      KEP-F03            TO   MEI-F03.
     MOVE      KEP-F04            TO   MEI-F04.
     MOVE      KEP-F05            TO   MEI-F05.
     MOVE      KEP-F06            TO   MEI-F06.
     MOVE      KEP-F07            TO   MEI-F07.
     MOVE      KEP-F08            TO   MEI-F08.
     MOVE      KEP-F09(1:4)       TO   WK-NOUHIN-YYYY.
     MOVE      KEP-F09(5:2)       TO   WK-NOUHIN-MM.
     MOVE      KEP-F09(7:2)       TO   WK-NOUHIN-DD.
     MOVE      "/"                TO   WK-KUGIRI1.
     MOVE      "/"                TO   WK-KUGIRI2.
     MOVE      WK-NOUHIN-BI       TO   MEI-F09.
     MOVE      KEP-F10            TO   MEI-F10.
     MOVE      KEP-F11            TO   MEI-F11.
     MOVE      KEP-F12            TO   MEI-F12.
     MOVE      KEP-F13            TO   MEI-F13.
     MOVE      KEP-F14            TO   MEI-F14.
     MOVE      KEP-F15            TO   MEI-F15.
     MOVE      KEP-F16            TO   MEI-F16.
     MOVE      KEP-F17            TO   MEI-F17.
     MOVE      KEP-F18            TO   MEI-F18.
     MOVE      KEP-F19            TO   MEI-F19.
     MOVE      KEP-F20            TO   MEI-F20.
*カンマセット
     MOVE      ","                TO   MEI-A01  MEI-A02  MEI-A03
                                       MEI-A04  MEI-A05  MEI-A06
                                       MEI-A07  MEI-A08  MEI-A09
                                       MEI-A10  MEI-A11  MEI-A12
                                       MEI-A13  MEI-A14  MEI-A15
                                       MEI-A16  MEI-A17  MEI-A18
                                       MEI-A19.
*制御バイト　２８　セット
     MOVE      X"28"              TO   MEI-F021.
     MOVE      X"28"              TO   MEI-F061.
     MOVE      X"28"              TO   MEI-F081.
     MOVE      X"29"              TO   MEI-F022.
     MOVE      X"29"              TO   MEI-F062.
     MOVE      X"29"              TO   MEI-F082.
*欠品ＣＳＶワークWRITE
     MOVE      SPACE              TO   EWK-REC.
*    INITIALIZE                        EWK-REC.
     MOVE      WK-MEISAI          TO   EWK-REC.
     WRITE  EWK-REC.
     ADD       1                  TO   WRT-CNT.
*欠品ＣＳＶデータ読込
     PERFORM  CSVKEPF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
******************************************************************
*              終了処理　　　　　　　　　　　　　　　　　　　　*
******************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"           TO   S-NAME.
*ファイルのクローズ
     CLOSE     CSVKEPF    CSVKEPWK.
*
     DISPLAY   "## RD-CNT  = " RD-CNT  " ##" UPON CONS.
     DISPLAY   "## WRT-CNT = " WRT-CNT " ##" UPON CONS.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
******************************************************************
*            ＥＤＩＣ支払明細書ワーク読込
******************************************************************
 CSVKEPF-READ-SEC           SECTION.
*
     MOVE    "CSVKEPF-READ-SEC"    TO     S-NAME.
*
     READ     CSVKEPF  AT  END
              MOVE    "END"      TO   END-FLG
              GO                 TO   CSVKEPF-READ-EXIT
     END-READ.
*
     ADD      1              TO   RD-CNT.
*
 CSVKEPF-READ-EXIT.
     EXIT.

```
