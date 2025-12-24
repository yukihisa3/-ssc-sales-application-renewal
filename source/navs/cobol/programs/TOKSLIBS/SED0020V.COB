******************************************************************
*     顧客名　　　　　　 ： (株)サカタのタネ殿
*     サブシステム名　　 ： ＥＤＩＣシステム
*     業務名　　　　　　 ： ＥＤＩＣシステム
*     モジュール名　　　 ： ＥＤＩＣ返品明細書ＣＳＶ出力
*     作成日／更新日　　 ： 2015/08/31
*     作成日／更新者　　 ：
*     処理概要　　　　　 ： ＥＤＩＣ返品明細ワークを読み、
*                           ＥＤＩＣ返品明細書ＣＳＶワークに
*                           データを出力する。
*     更新日／更新者　　 ：
*     更新日／更新者　　 ：
*     修正概要　　　　　 ：
*
******************************************************************
 IDENTIFICATION         DIVISION.
******************************************************************
 PROGRAM-ID.            SED0020V.
******************************************************************
 AUTHOR.                NAV.
 DATE-WRITTEN.          15/08/31.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
         CONSOLE   IS   CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*ＥＤＩＣ返品明細書ワーク
     SELECT   EWHENPF      ASSIGN    TO  DA-01-VI-EWHENPL1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      SEQUENTIAL
                           RECORD    KEY       HEP-F013
                                               HEP-F04
                                               HEP-F07
                                               HEP-F03
                                               HEP-F12
                           FILE      STATUS    HEP-ST.
*ＥＤＩＣ返品明細書ＣＳＶワーク
     SELECT   EWHENPWK     ASSIGN    TO  DA-01-S-EWHENPWK
                           FILE      STATUS    EWK-ST.
*
******************************************************************
 DATA                   DIVISION.
 FILE                   SECTION.
*ＥＤＩＣ返品明細書ワーク
 FD  EWHENPF.
     COPY        EWHENPF   OF        XFDLIB
     JOINING     HEP       AS        PREFIX.
*
*ＥＤＩＣ返品明細書ＣＳＶワーク
 FD  EWHENPWK           BLOCK        CONTAINS  1    RECORDS.
*
 01 HSF-REC.
    03  FILLER                    PIC  X(1000).
******************************************************************
 WORKING-STORAGE        SECTION.
*返品明細書ＣＳＶワーク
     COPY   EWHENPSF OF XFDLIB  JOINING   HEW  AS   PREFIX.
*
 01  WK-KOUMOKU.
     03  FILLER PIC  X(01)  VALUE X"28".
     03  FILLER PIC  N(03)  VALUE NC"受信日".
     03  FILLER PIC  X(01)  VALUE X"29".
     03  FILLER PIC  X(01)  VALUE ",".
     03  FILLER PIC  X(01)  VALUE X"28".
     03  FILLER PIC  N(04)  VALUE NC"受信時刻".
     03  FILLER PIC  X(01)  VALUE X"29".
     03  FILLER PIC  X(01)  VALUE ",".
     03  FILLER PIC  X(01)  VALUE X"28".
     03  FILLER PIC  N(06)  VALUE NC"取引先コード".
     03  FILLER PIC  X(01)  VALUE X"29".
     03  FILLER PIC  X(01)  VALUE ",".
     03  FILLER PIC  X(01)  VALUE X"28".
     03  FILLER PIC  N(04)  VALUE NC"取引先名".
     03  FILLER PIC  X(01)  VALUE X"29".
     03  FILLER PIC  X(01)  VALUE ",".
     03  FILLER PIC  X(01)  VALUE X"28".
     03  FILLER PIC  N(04)  VALUE NC"取引番号".
     03  FILLER PIC  X(01)  VALUE X"29".
     03  FILLER PIC  X(01)  VALUE ",".
     03  FILLER PIC  X(01)  VALUE X"28".
     03  FILLER PIC  N(08)  VALUE NC"最終納品先コード".
     03  FILLER PIC  X(01)  VALUE X"29".
     03  FILLER PIC  X(01)  VALUE ",".
     03  FILLER PIC  X(01)  VALUE X"28".
     03  FILLER PIC  N(07)  VALUE NC"最終納品先名称".
     03  FILLER PIC  X(01)  VALUE X"29".
     03  FILLER PIC  X(01)  VALUE ",".
     03  FILLER PIC  X(01)  VALUE X"28".
     03  FILLER PIC  N(09)  VALUE NC"最終納品先名称カナ".
     03  FILLER PIC  X(01)  VALUE X"29".
     03  FILLER PIC  X(01)  VALUE ",".
     03  FILLER PIC  X(01)  VALUE X"28".
     03  FILLER PIC  N(03)  VALUE NC"計上日".
     03  FILLER PIC  X(01)  VALUE X"29".
     03  FILLER PIC  X(01)  VALUE ",".
     03  FILLER PIC  X(01)  VALUE X"28".
     03  FILLER PIC  N(04)  VALUE NC"処理種別".
     03  FILLER PIC  X(01)  VALUE X"29".
     03  FILLER PIC  X(01)  VALUE ",".
     03  FILLER PIC  X(01)  VALUE X"28".
     03  FILLER PIC  N(05)  VALUE NC"処理種別名".
     03  FILLER PIC  X(01)  VALUE X"29".
     03  FILLER PIC  X(01)  VALUE ",".
     03  FILLER PIC  X(01)  VALUE X"28".
     03  FILLER PIC  N(03)  VALUE NC"税区分".
     03  FILLER PIC  X(01)  VALUE X"29".
     03  FILLER PIC  X(01)  VALUE ",".
     03  FILLER PIC  X(01)  VALUE X"28".
     03  FILLER PIC  N(04)  VALUE NC"税区分名".
     03  FILLER PIC  X(01)  VALUE X"29".
     03  FILLER PIC  X(01)  VALUE ",".
     03  FILLER PIC  X(01)  VALUE X"28".
     03  FILLER PIC  N(06)  VALUE NC"取引明細番号".
     03  FILLER PIC  X(01)  VALUE X"29".
     03  FILLER PIC  X(01)  VALUE ",".
     03  FILLER PIC  X(01)  VALUE X"28".
     03  FILLER PIC  N(05)  VALUE NC"自由使用欄".
     03  FILLER PIC  X(01)  VALUE X"29".
     03  FILLER PIC  X(01)  VALUE ",".
     03  FILLER PIC  X(01)  VALUE X"28".
     03  FILLER PIC  N(05)  VALUE NC"商品コード".
     03  FILLER PIC  X(01)  VALUE X"29".
     03  FILLER PIC  X(01)  VALUE ",".
     03  FILLER PIC  X(01)  VALUE X"28".
     03  FILLER PIC  N(05)  VALUE NC"商品名カナ".
     03  FILLER PIC  X(01)  VALUE X"29".
     03  FILLER PIC  X(01)  VALUE ",".
     03  FILLER PIC  X(01)  VALUE X"28".
     03  FILLER PIC  N(05)  VALUE NC"規格名カナ".
     03  FILLER PIC  X(01)  VALUE X"29".
     03  FILLER PIC  X(01)  VALUE ",".
     03  FILLER PIC  X(01)  VALUE X"28".
     03  FILLER PIC  N(04)  VALUE NC"原価金額".
     03  FILLER PIC  X(01)  VALUE X"29".
     03  FILLER PIC  X(01)  VALUE ",".
     03  FILLER PIC  X(01)  VALUE X"28".
     03  FILLER PIC  N(03)  VALUE NC"原単価".
     03  FILLER PIC  X(01)  VALUE X"29".
     03  FILLER PIC  X(01)  VALUE ",".
     03  FILLER PIC  X(01)  VALUE X"28".
     03  FILLER PIC  N(04)  VALUE NC"売価金額".
     03  FILLER PIC  X(01)  VALUE X"29".
     03  FILLER PIC  X(01)  VALUE ",".
     03  FILLER PIC  X(01)  VALUE X"28".
     03  FILLER PIC  N(03)  VALUE NC"売単価".
     03  FILLER PIC  X(01)  VALUE X"29".
     03  FILLER PIC  X(01)  VALUE ",".
     03  FILLER PIC  X(01)  VALUE X"28".
     03  FILLER PIC  N(08)  VALUE NC"返品数量（バラ）".
     03  FILLER PIC  X(01)  VALUE X"29".
     03  FILLER PIC  X(01)  VALUE ",".
     03  FILLER PIC  X(01)  VALUE X"28".
     03  FILLER PIC  N(10)  VALUE NC"返品・値引理由コード".
     03  FILLER PIC  X(01)  VALUE X"29".
     03  FILLER PIC  X(01)  VALUE ",".
     03  FILLER PIC  X(01)  VALUE X"28".
     03  FILLER PIC  N(08)  VALUE NC"返品値引理由内容".
     03  FILLER PIC  X(01)  VALUE X"29".
* 2023/11/19 ARK ADD START
     03  FILLER PIC  X(01)  VALUE ",".
     03  FILLER PIC  X(01)  VALUE X"28".
     03  FILLER PIC  N(05)  VALUE NC"譲渡年月１".
     03  FILLER PIC  X(01)  VALUE X"29".
     03  FILLER PIC  X(01)  VALUE ",".
     03  FILLER PIC  X(01)  VALUE X"28".
     03  FILLER PIC  N(05)  VALUE NC"譲渡年月１".
     03  FILLER PIC  X(01)  VALUE X"29".
* 2023/11/19 ARK ADD END

*ワーク項目
 01  END-FLG                      PIC  X(03)     VALUE  SPACE.
 01  RD-CNT                       PIC  9(08)     VALUE  ZERO.
 01  WRT-CNT                      PIC  9(08)     VALUE  ZERO.
*小数点編集
 01  WK-HENSYU1.
     03  WK-10-KETA               PIC  9(08)V9(02).
     03  WK-10-KETA-R             REDEFINES      WK-10-KETA.
         05  WK-8-KETA            PIC  9(08).
         05  WK-2-KETA            PIC  9(02).
 01  WK-HENSYU2.
     03  WK-7-KETA                PIC  9(06)V9(01).
     03  WK-7-KETA-R              REDEFINES      WK-7-KETA.
         05  WK-6-KETA            PIC  9(06).
         05  WK-1-KETA            PIC  9(01).
*プログラムＳＴＡＴＵＳ
 01  WK-ST.
     03  HEP-ST                   PIC  X(02).
     03  EWK-ST                   PIC  X(02).
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
     03  HEP-ERR                  PIC  N(20)     VALUE
         NC"ＥＤＩＣ返品明細書ワークエラ－".
     03  EWK-ERR                  PIC  N(20)     VALUE
         NC"ＥＤＩＣ返品明細書ＣＳＶワークエラ－".
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER               PIC  X(05)     VALUE " *** ".
         05  ST-PG                PIC  X(08)     VALUE "SED0020V".
         05  FILLER               PIC  X(11)     VALUE
                                        " START *** ".
     03  MSG-END.
         05  FILLER               PIC  X(05)     VALUE " *** ".
         05  ST-PG                PIC  X(08)     VALUE "SED0020V".
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
 HEP-ERR-SEC                SECTION.
     USE      AFTER         EXCEPTION PROCEDURE  EWHENPF.
     DISPLAY       HEP-ERR  UPON      CONS.
     DISPLAY       SEC-NAME UPON      CONS.
     DISPLAY       HEP-ST   UPON      CONS.
     MOVE          "4000"   TO        PROGRAM-STATUS.
     STOP          RUN.
 HWK-ERR-SEC                SECTION.
     USE      AFTER         EXCEPTION PROCEDURE  EWHENPWK.
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
     OPEN     INPUT     EWHENPF.
     OPEN     OUTPUT    EWHENPWK.
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
*ＥＤＩＣ返品明細書ワーク読込
     PERFORM  EWHENPF-READ-SEC.
*終了チェック（存在した場合は、タイトル行出力）
     IF  END-FLG  NOT =  "END"
         MOVE   SPACE             TO        HSF-REC
*        INITIALIZE                         HSF-REC
         MOVE   WK-KOUMOKU        TO        HSF-REC
         WRITE  HSF-REC
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
     MOVE      SPACE              TO   HEW-REC.
     INITIALIZE                        HEW-REC.
*ファイル内容を転送
     MOVE      HEP-F011           TO   HEW-F011.
     MOVE      HEP-F012           TO   HEW-F012.
     MOVE      HEP-F013           TO   HEW-F013.
     MOVE      HEP-F02            TO   HEW-F02.
     MOVE      HEP-F03            TO   HEW-F03.
     MOVE      HEP-F04            TO   HEW-F04.
     MOVE      HEP-F05            TO   HEW-F05.
     MOVE      HEP-F06            TO   HEW-F06.
     MOVE      HEP-F07            TO   HEW-F07.
     MOVE      HEP-F08            TO   HEW-F08.
     MOVE      HEP-F09            TO   HEW-F09.
     MOVE      HEP-F10            TO   HEW-F10.
     MOVE      HEP-F11            TO   HEW-F11.
     MOVE      HEP-F12            TO   HEW-F12.
     MOVE      HEP-F13            TO   HEW-F13.
     MOVE      HEP-F14            TO   HEW-F14.
     MOVE      HEP-F15            TO   HEW-F15.
     MOVE      HEP-F16            TO   HEW-F16.
     MOVE      HEP-F17            TO   HEW-F17.
     MOVE      HEP-F18            TO   WK-10-KETA.
     MOVE      WK-8-KETA          TO   HEW-F18(1:8).
     MOVE      "."                TO   HEW-F18(9:1).
     MOVE      WK-2-KETA          TO   HEW-F18(10:2).
     MOVE      HEP-F19            TO   HEW-F19.
     MOVE      HEP-F20            TO   HEW-F20.
     MOVE      HEP-F21            TO   WK-7-KETA.
     MOVE      WK-6-KETA          TO   HEW-F21(1:6).
     MOVE      "."                TO   HEW-F21(7:1).
     MOVE      WK-1-KETA          TO   HEW-F21(8:1).
     MOVE      HEP-F22            TO   HEW-F22.
     MOVE      HEP-F23            TO   HEW-F23.
* 2023/11/19 ARK ADD START
     MOVE      HEP-F24            TO   HEW-F24.
     MOVE      HEP-F25            TO   HEW-F25.
* 2023/11/19 ARK ADD END
*カンマセット
     MOVE      ","                TO   HEW-A01  HEW-A02  HEW-A05
                                       HEW-A08  HEW-A09  HEW-A14
                                       HEW-A16  HEW-A17  HEW-A18
                                       HEW-A19  HEW-A21  HEW-A22
                                       HEW-A15  HEW-A20  HEW-A23.
* 2023/11/19 ARK ADD START
     MOVE      ","                TO   HEW-A26.
* 2023/11/19 ARK ADD END
*制御バイト　２８　セット
     MOVE       ","               TO   HEW-A03(1:1).
     MOVE       ","               TO   HEW-A06(1:1).
     MOVE       ","               TO   HEW-A10(1:1).
     MOVE       ","               TO   HEW-A12(1:1).
     MOVE       ","               TO   HEW-A24(1:1).
     MOVE      X"28"              TO   HEW-A03(2:1).
     MOVE      X"28"              TO   HEW-A06(2:1).
     MOVE      X"28"              TO   HEW-A10(2:1).
     MOVE      X"28"              TO   HEW-A12(2:1).
     MOVE      X"28"              TO   HEW-A24(2:1).
     MOVE      X"29"              TO   HEW-A04(1:1).
     MOVE      X"29"              TO   HEW-A07(1:1).
     MOVE      X"29"              TO   HEW-A11(1:1).
     MOVE      X"29"              TO   HEW-A13(1:1).
     MOVE      X"29"              TO   HEW-A25(1:1).
     MOVE       ","               TO   HEW-A04(2:1).
     MOVE       ","               TO   HEW-A07(2:1).
     MOVE       ","               TO   HEW-A11(2:1).
     MOVE       ","               TO   HEW-A13(2:1).
* 2023/11/19 ARK ADD START
     MOVE      ","                TO   HEW-A25(2:1).
* 2023/11/19 ARK ADD END
*ＥＤＩＣ返品明細書ワークWRITE
     MOVE      SPACE              TO   HSF-REC.
*    INITIALIZE                        HSF-REC.
     MOVE      HEW-REC            TO   HSF-REC.
     WRITE  HSF-REC.
     ADD       1                  TO   WRT-CNT.
*ＥＤＩＣ返品明細書ワーク読込
     PERFORM  EWHENPF-READ-SEC.
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
     CLOSE     EWHENPF    EWHENPWK.
*
     DISPLAY   "## RD-CNT  = " RD-CNT  " ##" UPON CONS.
     DISPLAY   "## WRT-CNT = " WRT-CNT " ##" UPON CONS.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
******************************************************************
*            ＥＤＩＣ返品明細書ワーク読込
******************************************************************
 EWHENPF-READ-SEC           SECTION.
*
     MOVE    "EWHENPF-READ-SEC"    TO     S-NAME.
*
     READ     EWHENPF  AT  END
              MOVE    "END"      TO   END-FLG
              GO                 TO   EWHENPF-READ-EXIT
     END-READ.
*
     ADD      1              TO   RD-CNT.
*
 EWHENPF-READ-EXIT.
     EXIT.
