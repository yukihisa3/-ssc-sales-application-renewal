# SBT0420L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SBT0420L.COB`

## ソースコード

```cobol
****************************************************************
*
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　
*    サブシステム　　　　：　ＬＩＮＫＳ入出荷連携
*    業務名　　　　　　　：　出荷業務（流通ＢＭＳ）　
*    モジュール名　　　　：　出荷梱包データ件数リスト
*    　　　　　　　　　　　　カトーレックＬＩＮＫＳ
*    作成日／作成者　　　：　2014/08/11
*    処理概要　　　　　　：　出荷梱包取込件数をパラメータで　　
*    　　　　　　　　　　：  受取り、リスト出力を行う。
*    更新履歴　　　　　　：
*
****************************************************************
 IDENTIFICATION            DIVISION.
 PROGRAM-ID.               SBT0420L.
 AUTHOR.                   NAV.
 DATE-WRITTEN.             2014/08/11.
 ENVIRONMENT               DIVISION.
 CONFIGURATION             SECTION.
 SOURCE-COMPUTER.
 OBJECT-COMPUTER.
 SPECIAL-NAMES.
     STATION     IS        STA
     YA          IS        NIHONGO
     YB          IS        YB
     YA-21       IS        YA-21
     YA-22       IS        YA-22
     YB-21       IS        YB-21
     YB-22       IS        YB-22
     CONSOLE     IS        CONS.
***********************************************************
*             INPUT-OUTPUT                                *
***********************************************************
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*担当者マスタ
     SELECT     TANMS1     ASSIGN    TO        TANMS1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       TAN-F01
                                               TAN-F02
                           FILE      STATUS    TAN-ST.
*倉庫マスタ
     SELECT     ZSOKMS1    ASSIGN    TO        ZSOKMS1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       SOK-F01
                           FILE      STATUS    SOK-ST.
*取引先マスタ
     SELECT     TOKMS2     ASSIGN    TO        TOKMS2
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       TOK-F01
                           FILE      STATUS    TOK-ST.
*%* プリンター *%*
     SELECT     PRTF       ASSIGN    TO        LP-04.
******************************************************************
*             DATA                DIVISION                       *
******************************************************************
 DATA                      DIVISION.
 FILE                      SECTION.
*倉庫マスタ
 FD  ZSOKMS1
     LABEL       RECORD    IS        STANDARD.
     COPY        ZSOKMS1   OF        XFDLIB
     JOINING     SOK       AS        PREFIX.

*担当者マスタ
 FD  TANMS1
     LABEL       RECORD    IS        STANDARD.
     COPY        TANMS1    OF        XFDLIB
     JOINING     TAN       AS        PREFIX.
*取引先マスタ
 FD  TOKMS2
     LABEL       RECORD    IS        STANDARD.
     COPY        TOKMS2    OF        XFDLIB
     JOINING     TOK       AS        PREFIX.
*プリンター
 FD    PRTF      LINAGE  IS  66.
 01    P-REC                 PIC  X(200).
******************************************************************
 WORKING-STORAGE           SECTION.
******************************************************************
*
 01  FILE-STATUS.
     03  PRT-ST            PIC X(02).
     03  PRT-ST1           PIC X(04).
     03  SOK-ST            PIC X(02).
     03  TAN-ST            PIC X(02).
     03  TOK-ST            PIC X(02).
*年度
 01  YYYY.
     03  YY1               PIC  9(02).
     03  YY2               PIC  9(02).
 01  WK-AREA.
     03  END-SW            PIC 9(01) VALUE     ZERO.
     03  ERR-SW            PIC 9(01) VALUE     ZERO.
     03  PAGE-CNT          PIC 9(05) VALUE     ZERO.
     03  LINE-CNT          PIC 9(05) VALUE     ZERO.
     03  WK-NYUKIN         PIC 9(02) VALUE     ZERO.
*
*日付／時刻
 01  TIME-AREA.
     03  WK-TIME                  PIC  9(08)  VALUE  ZERO.
 01  DATE-AREA.
     03  WK-YS                    PIC  9(02)  VALUE  ZERO.
     03  WK-DATE.
         05  WK-Y                 PIC  9(02)  VALUE  ZERO.
         05  WK-M                 PIC  9(02)  VALUE  ZERO.
         05  WK-D                 PIC  9(02)  VALUE  ZERO.
 01  DATE-AREAR2       REDEFINES      DATE-AREA.
     03  SYS-DATE                 PIC  9(08).
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*
 01  IN-DATA               PIC X(01).
 01  FILE-ERR.
     03  TAN-ERR           PIC N(10) VALUE
                        NC"担当者マスタエラー".
     03  PRT-ERR           PIC N(10) VALUE
                        NC"プリンターエラー".
     03  SOK-ERR           PIC N(10) VALUE
                        NC"倉庫マスタエラー".
     03  TOK-ERR           PIC N(10) VALUE
                        NC"取引先マスタエラー".
*帳票表示日付編集
 01  HEN-DATE.
     03  HEN-DATE-YYYY            PIC  9(04)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-DD              PIC  9(02)  VALUE  ZERO.
*帳票表示時刻編集
 01  HEN-TIME.
     03  HEN-TIME-HH              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-SS              PIC  9(02)  VALUE  ZERO.
*退避エリア
 01  WK-BMNCD-BK           PIC  X(04).
 01  WK-TANCD-BK           PIC  X(02).
 01  WK-TANNM-BK           PIC  N(10).
 01  WK-TORCD-BK           PIC  X(08).
 01  WK-TORNM-BK           PIC  N(15).
*変換エリア
 01  TB-CVT-SU.
     03  TB-CVT-SU          PIC  X(11)  VALUE
         "0123456789 ".
     03  TB-CVT-SUR  REDEFINES TB-CVT-SU
                            PIC  X(01)  OCCURS 11.

     03  TB-CVT-SU-N        PIC  N(11)  VALUE
       NC"０１２３４５６７８９　".
     03  TB-CVT-SU-NR  REDEFINES TB-CVT-SU-N
                            PIC  N(01)  OCCURS 11.

 01  WK-KENSU-X.
     03  WK-KENSU          PIC  9(08) VALUE  ZERO.
 01  WK-HENKAN.
     03  WK-HENKAN-N       PIC  N(08).
     03  WK-HENKAN-NR  REDEFINES  WK-HENKAN-N
                           PIC  N(01)  OCCURS 8.
 01  WK-SHUKA-X.
     03  WK-SHUKA-G        PIC  9(08) VALUE  ZERO.
 01  WK-NYUKA-X.
     03  WK-NYUKA-G        PIC  9(08) VALUE  ZERO.
*更新範囲
 01  TRND-DT.
     03  TRND-DATE         PIC  9(08).
     03  TRND-TIME         PIC  9(06).
 01  TO-DT.
     03  TO-DATE           PIC  9(08).
     03  TO-TIME           PIC  9(06).
*
 01  IX                    PIC  9(04) VALUE 0.
 01  READ-CNT              PIC  9(07) VALUE 0.
 01  IN-CNT                PIC  9(07) VALUE 0.
 01  FG-ZSOKMS1-INV        PIC  X(03) VALUE SPACE.
 01  FG-TANMS1-INV         PIC  X(03) VALUE SPACE.
 01  FG-TOKMS2-INV         PIC  X(03) VALUE SPACE.
 01  SET-FLG               PIC  X(03) VALUE SPACE.
 01  HENKAN-FLG            PIC  X(03) VALUE SPACE.
*帳票出力定義エリア
****  見出し行１             ****
 01  MIDASI-1.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  X(08)  VALUE  "SBT0420L".
     02  FILLER              PIC  X(23)  VALUE  SPACE.
     02  FILLER              PIC  N(14)
         CHARACTER  TYPE  IS  YA-22      VALUE
         NC"＜出荷梱包データ件数リスト＞".
     02  FILLER              PIC  X(26)  VALUE  SPACE.
     02  SYSYY               PIC  9999.
     02  FILLER              PIC  N(01)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"年".
     02  SYSMM               PIC  99.
     02  FILLER              PIC  N(01)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"月".
     02  SYSDD               PIC  99.
     02  FILLER              PIC  N(01)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"日".
     02  FILLER              PIC  X(02) VALUE  SPACE.
     02  LPAGE               PIC  ZZ9.
     02  FILLER              PIC  N(01)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"頁".
****  見出し行２***
 01  MIDASI-2.
     02  FILLER              PIC  X(116) VALUE  SPACE.
     02  TIMEHH              PIC  99.
     02  FILLER              PIC  X(01)  VALUE  ":".
     02  TIMEMM              PIC  99.
     02  FILLER              PIC  X(01)  VALUE  ":".
     02  TIMESS              PIC  99.
****  線１           ****
 01  HASEN-1.
     02  FILLER              PIC  X(136) VALUE  ALL "-".
****  線２           ****
 01  HASEN-2.
     02  FILLER              PIC  X(21) VALUE  SPACE.
     02  FILLER              PIC  X(66) VALUE  ALL "-".

****  明細行１               ****
 01  MEISAI-1.
     02  FILLER              PIC  X(05)  VALUE  SPACE.
     02  FILLER              PIC  N(19)
         CHARACTER  TYPE  IS  YB-22      VALUE
         NC"以下の出荷梱包データを取り込みました。".
****  明細行２               ****
 01  MEISAI-2.
     02  FILLER              PIC  X(17) VALUE  SPACE.
     02  FILLER              PIC  N(05) VALUE
*        NC"担当者　："      CHARACTER  TYPE  IS  YB.
         NC"担当者　："      CHARACTER  TYPE  IS  YB-21.
     02  FILLER              PIC  X(01) VALUE  SPACE.
     02  BMNCD               PIC  X(04).
     02  FILLER              PIC  X(01) VALUE  "-".
     02  TANCD               PIC  X(02).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
*    02  TANNM               PIC  N(10) CHARACTER  TYPE  IS  YB.
     02  TANNM               PIC  N(10) CHARACTER  TYPE  IS YB-21.
****  明細行３               ****
 01  MEISAI-3.
     02  FILLER              PIC  X(17) VALUE  SPACE.
     02  FILLER              PIC  N(05) VALUE
*        NC"取込倉庫："      CHARACTER  TYPE  IS  YB.
         NC"取込倉庫："      CHARACTER  TYPE  IS  YB-21.
     02  FILLER              PIC  X(01) VALUE  SPACE.
     02  SOKOCD              PIC  X(02).
     02  FILLER              PIC  X(06) VALUE  SPACE.
*    02  SOKONM              PIC  N(18) CHARACTER  TYPE  IS  YB.
     02  SOKONM              PIC  N(18) CHARACTER  TYPE  IS YB-21.
****  明細行４               ****
 01  MEISAI-4.
     02  FILLER              PIC  X(17) VALUE  SPACE.
     02  FILLER              PIC  N(05) VALUE
*        NC"取引先　："      CHARACTER  TYPE  IS  YB.
         NC"取引先　："      CHARACTER  TYPE  IS  YB-21.
     02  FILLER              PIC  X(01) VALUE  SPACE.
     02  TOKCD               PIC  X(06).
     02  FILLER              PIC  X(02) VALUE  SPACE.
*    02  TOKNM               PIC  N(15) CHARACTER  TYPE  IS  YB.
     02  TOKNM               PIC  N(15) CHARACTER  TYPE  IS YB-21.
****  明細行５               ****
*01  MEISAI-5.
*    02  FILLER              PIC  X(17) VALUE  SPACE.
*    02  FILLER              PIC  N(05) VALUE
*        NC"抽出倉庫："  CHARACTER  TYPE  IS  YB-21.
*    02  FILLER              PIC  X(01) VALUE  SPACE.
*    02  SOKOCD              PIC  X(02).
*    02  FILLER              PIC  X(01) VALUE  SPACE.
*    02  SOKONM              PIC  N(18) CHARACTER  TYPE  IS YB-21.
****  明細行６               ****
 01  MEISAI-6.
     02  FILLER              PIC  X(17)  VALUE  SPACE.
     02  FILLER              PIC  N(04)  VALUE
*        NC"取込件数" CHARACTER  TYPE  IS  YB.
         NC"取込件数" CHARACTER  TYPE  IS  YB-21.
****  明細行７               ****
 01  MEISAI-7.
     02  FILLER              PIC  X(21) VALUE  SPACE.
     02  FILLER              PIC  N(11) VALUE
     NC"メッセージヘッダ　　　" CHARACTER TYPE IS YB-21.
     02  FILLER              PIC  X(02) VALUE  SPACE.
     02  FILLER              PIC  N(01) VALUE
     NC"：" CHARACTER TYPE IS YB-21.
     02  FILLER              PIC  X(01) VALUE  SPACE.
     02  MSG-A               PIC  N(08) CHARACTER TYPE IS YB-21.
     02  FILLER              PIC  N(01) VALUE
         NC"件"           CHARACTER TYPE IS YB-21.
****  明細行８               ****
 01  MEISAI-8.
     02  FILLER              PIC  X(21) VALUE  SPACE.
     02  FILLER              PIC  N(11) VALUE
     NC"出荷梱包リスト　　　　" CHARACTER TYPE IS YB-21.
     02  FILLER              PIC  X(02) VALUE  SPACE.
     02  FILLER              PIC  N(01) VALUE
     NC"：" CHARACTER TYPE IS YB-21.
     02  FILLER              PIC  X(01) VALUE  SPACE.
     02  MSG-B               PIC  N(08) CHARACTER TYPE IS YB-21.
     02  FILLER              PIC  N(01) VALUE
         NC"件"           CHARACTER TYPE IS YB-21.
****  明細行９               ****
 01  MEISAI-9.
     02  FILLER              PIC  X(21) VALUE  SPACE.
     02  FILLER              PIC  N(11) VALUE
     NC"出荷梱包リスト発注元別" CHARACTER TYPE IS YB-21.
     02  FILLER              PIC  X(02) VALUE  SPACE.
     02  FILLER              PIC  N(01) VALUE
     NC"：" CHARACTER TYPE IS YB-21.
     02  FILLER              PIC  X(01) VALUE  SPACE.
     02  MSG-C               PIC  N(08) CHARACTER TYPE IS YB-21.
     02  FILLER              PIC  N(01) VALUE
         NC"件"           CHARACTER TYPE IS YB-21.
****  明細行10               ****
 01  MEISAI-10.
     02  FILLER              PIC  X(21) VALUE  SPACE.
     02  FILLER              PIC  N(11) VALUE
     NC"出荷梱包内容　　　　　" CHARACTER TYPE IS YB-21.
     02  FILLER              PIC  X(02) VALUE  SPACE.
     02  FILLER              PIC  N(01) VALUE
     NC"：" CHARACTER TYPE IS YB-21.
     02  FILLER              PIC  X(01) VALUE  SPACE.
     02  MSG-D               PIC  N(08) CHARACTER TYPE IS YB-21.
     02  FILLER              PIC  N(01) VALUE
         NC"件"           CHARACTER TYPE IS YB-21.
****  明細行11               ****
 01  MEISAI-11.
     02  FILLER              PIC  X(21) VALUE  SPACE.
     02  FILLER              PIC  N(11) VALUE
     NC"取引明細　　　　　　　" CHARACTER TYPE IS YB-21.
     02  FILLER              PIC  X(02) VALUE  SPACE.
     02  FILLER              PIC  N(01) VALUE
     NC"：" CHARACTER TYPE IS YB-21.
     02  FILLER              PIC  X(01) VALUE  SPACE.
     02  MSG-E               PIC  N(08) CHARACTER TYPE IS YB-21.
     02  FILLER              PIC  N(01) VALUE
         NC"件"           CHARACTER TYPE IS YB-21.
****  明細行12               ****
 01  MEISAI-12.
     02  FILLER              PIC  X(21) VALUE  SPACE.
     02  FILLER              PIC  N(11) VALUE
     NC"ＩＴＦ情報　　　　　　" CHARACTER TYPE IS YB-21.
     02  FILLER              PIC  X(02) VALUE  SPACE.
     02  FILLER              PIC  N(01) VALUE
     NC"：" CHARACTER TYPE IS YB-21.
     02  FILLER              PIC  X(01) VALUE  SPACE.
     02  MSG-F               PIC  N(08) CHARACTER TYPE IS YB-21.
     02  FILLER              PIC  N(01) VALUE
         NC"件"           CHARACTER TYPE IS YB-21.
****  明細行13               ****
 01  MEISAI-13.
     02  FILLER              PIC  X(21) VALUE  SPACE.
     02  FILLER              PIC  N(11) VALUE
     NC"欠品情報　　　　　　　" CHARACTER TYPE IS YB-21.
     02  FILLER              PIC  X(02) VALUE  SPACE.
     02  FILLER              PIC  N(01) VALUE
     NC"：" CHARACTER TYPE IS YB-21.
     02  FILLER              PIC  X(01) VALUE  SPACE.
     02  MSG-G               PIC  N(08) CHARACTER TYPE IS YB-21.
     02  FILLER              PIC  N(01) VALUE
         NC"件"           CHARACTER TYPE IS YB-21.
****  明細行１４             ****
 01  MEISAI-14.
     02  FILLER              PIC  X(22) VALUE  SPACE.
     02  FILLER              PIC  N(12) VALUE
     NC"　　　　合　　計　　　　" CHARACTER TYPE IS YB-21.
     02  FILLER              PIC  X(02) VALUE  SPACE.
     02  SGOK                PIC  N(08) CHARACTER TYPE IS YB-21.
     02  FILLER              PIC  N(01) VALUE NC"件"
     CHARACTER TYPE IS YB-21.
*
 LINKAGE                   SECTION.
 01  PAR-TOKCD            PIC X(08).
 01  PAR-SOKOCD           PIC X(02).
 01  PAR-TANCD            PIC X(02).
 01  PAR-BUMON            PIC X(04).
 01  PAR-KENSU1           PIC X(08).
 01  PAR-KENSU2           PIC X(08).
 01  PAR-KENSU3           PIC X(08).
 01  PAR-KENSU4           PIC X(08).
 01  PAR-KENSU5           PIC X(08).
 01  PAR-KENSU6           PIC X(08).
 01  PAR-KENSU7           PIC X(08).
 01  PAR-KENSUA2          PIC X(08).
******************************************************************
*             PROCEDURE           DIVISION                       *
******************************************************************
 PROCEDURE                 DIVISION USING PAR-TOKCD
                                          PAR-SOKOCD
                                          PAR-TANCD
                                          PAR-BUMON
                                          PAR-KENSU1  PAR-KENSU2
                                          PAR-KENSU3  PAR-KENSU4
                                          PAR-KENSU5  PAR-KENSU6
                                          PAR-KENSU7.
*
 DECLARATIVES.
 PRT-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE PRTF.
     DISPLAY     PRT-ERR   UPON      STA.
     DISPLAY     PRT-ST    UPON      STA.
*?   ACCEPT      IN-DATA   FROM      STA.
     STOP        RUN.
 SOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE ZSOKMS1.
     DISPLAY     SOK-ERR   UPON      STA.
     DISPLAY     SOK-ST    UPON      STA.
*?   ACCEPT      IN-DATA   FROM      STA.
     MOVE        4000      TO        PROGRAM-STATUS.
     STOP        RUN.
 TAN-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE TANMS1.
     DISPLAY     TAN-ERR   UPON      STA.
     DISPLAY     TAN-ST    UPON      STA.
*?   ACCEPT      IN-DATA   FROM      STA.
     MOVE        4000      TO        PROGRAM-STATUS.
     STOP        RUN.
 TOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE TOKMS2.
     DISPLAY     TOK-ERR   UPON      STA.
     DISPLAY     TOK-ST    UPON      STA.
*?   ACCEPT      IN-DATA   FROM      STA.
     MOVE        4000      TO        PROGRAM-STATUS.
     STOP        RUN.
 END DECLARATIVES.
***********************************************************
*                     ＭＡＩＮ処理                        *
***********************************************************
 PROC-SEC                  SECTION.
*
 PROC-010.
     PERFORM     INIT-SEC.
     PERFORM     MAIN-SEC.
     PERFORM     END-SEC.
*
     STOP        RUN.
 PROC-EXIT.
     EXIT.
**********************************************************
*                      Ｉ Ｎ Ｉ Ｔ                       *
**********************************************************
 INIT-SEC                  SECTION.
     MOVE        ZERO      TO        PAGE-CNT.
     MOVE        61        TO        LINE-CNT.
*
*システム日付・時刻の取得
     ACCEPT   WK-DATE           FROM   DATE.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     WK-DATE             TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD       TO   DATE-AREA.
*システム時刻取得
     ACCEPT    WK-TIME          FROM   TIME.
*
     OPEN        INPUT     ZSOKMS1 TANMS1 TOKMS2.
     OPEN        OUTPUT    PRTF.
*
 INIT-EXIT.
     EXIT.
***********************************************************
*                     ＭＡＩＮ処理                        *
***********************************************************
 MAIN-SEC                  SECTION.
*
     PERFORM     HEAD-SET-SEC.
     PERFORM     MEIEDT-SEC.
     PERFORM     MIDA-SEC.
     PERFORM     MEIWRT-SEC.
*
 MAIN-EXIT.
     EXIT.
**********************************************************
*                       Ｅ Ｎ Ｄ                         *
**********************************************************
 END-SEC                   SECTION.
*
     CLOSE       PRTF    ZSOKMS1 TANMS1 TOKMS2.
 END-EXIT.
     EXIT.
**********************************************************
*                 ヘッダデータ編集                      *
**********************************************************
 HEAD-SET-SEC                  SECTION.
*
     MOVE        WK-YS     TO        YY1.
     MOVE        WK-Y      TO        YY2.
     MOVE        YYYY      TO        SYSYY.
     MOVE        WK-M      TO        SYSMM.
     MOVE        WK-D      TO        SYSDD.
     MOVE        WK-TIME(1:2)  TO    TIMEHH.
     MOVE        WK-TIME(3:2)  TO    TIMEMM.
     MOVE        WK-TIME(5:2)  TO    TIMESS.
*
*
 HEAD-SET-EXIT.
     EXIT.
**********************************************************
*                 見出しデータ編集書き出し               *
**********************************************************
 MIDA-SEC                  SECTION.
*
     IF  PAGE-CNT  >  ZERO
         MOVE       SPACE   TO      P-REC
         WRITE      P-REC   AFTER   PAGE
     END-IF.
*
     ADD         1         TO        PAGE-CNT.
     MOVE        ZERO      TO        LINE-CNT.
     MOVE        PAGE-CNT  TO        LPAGE.
**************
*帳票書き出し*
**************
     MOVE       SPACE   TO  P-REC.
     WRITE      P-REC   AFTER  2.
     WRITE      P-REC   FROM    MIDASI-1   AFTER  1.
     WRITE      P-REC   FROM    MIDASI-2   AFTER  1.
     WRITE      P-REC   FROM    HASEN-1    AFTER  1.
*
*    ADD         10        TO        LINE-CNT.
*
 MIDA-EXIT.
     EXIT.
**********************************************************
*                 明細情報の編集                         *
**********************************************************
 MEIEDT-SEC                  SECTION.
*担当者
     MOVE        PAR-BUMON   TO    BMNCD   TAN-F01.
     MOVE        PAR-TANCD   TO    TANCD   TAN-F02.
     PERFORM     RD-TANMS1-SEC.
     IF FG-TANMS1-INV = ZERO
        MOVE  TAN-F03      TO  TANNM
     END-IF.
*倉庫
     MOVE        PAR-SOKOCD  TO    SOKOCD  SOK-F01.
     PERFORM     RD-ZSOKMS1-SEC.
     IF FG-ZSOKMS1-INV = ZERO
        MOVE  SOK-F02      TO  SOKONM
     END-IF.
*取引先
     MOVE        PAR-TOKCD(3:6)  TO    TOKCD.
     MOVE        PAR-TOKCD       TO    TOK-F01.
     PERFORM     RD-TOKMS2-SEC.
     IF FG-TOKMS2-INV = ZERO
        MOVE  TOK-F02      TO  TOKNM
     END-IF.
*取込件数
*    メッセージヘッダ
     MOVE    PAR-KENSU1   TO   WK-KENSU.
     PERFORM     ZENKAKU-SEC.
     MOVE  WK-HENKAN-N  TO  MSG-A.
     COMPUTE  WK-SHUKA-G = WK-SHUKA-G + WK-KENSU.
*    出荷梱包リスト
     MOVE    PAR-KENSU2   TO   WK-KENSU.
     PERFORM     ZENKAKU-SEC.
     MOVE  WK-HENKAN-N  TO  MSG-B.
     COMPUTE  WK-SHUKA-G = WK-SHUKA-G + WK-KENSU.
*    出荷梱包リスト発注元別
     MOVE    PAR-KENSU3   TO   WK-KENSU.
     PERFORM     ZENKAKU-SEC.
     MOVE  WK-HENKAN-N  TO  MSG-C.
     COMPUTE  WK-SHUKA-G = WK-SHUKA-G + WK-KENSU.
*    出荷梱包内容　　　　　
     MOVE    PAR-KENSU4   TO   WK-KENSU.
     PERFORM     ZENKAKU-SEC.
     MOVE  WK-HENKAN-N  TO  MSG-D.
     COMPUTE  WK-SHUKA-G = WK-SHUKA-G + WK-KENSU.
*    取引明細
     MOVE    PAR-KENSU5   TO   WK-KENSU.
     PERFORM     ZENKAKU-SEC.
     MOVE  WK-HENKAN-N  TO  MSG-E.
     COMPUTE  WK-SHUKA-G = WK-SHUKA-G + WK-KENSU.
*    ＩＴＦ情報　　
     MOVE    PAR-KENSU6   TO   WK-KENSU.
     PERFORM     ZENKAKU-SEC.
     MOVE  WK-HENKAN-N  TO  MSG-F.
     COMPUTE  WK-SHUKA-G = WK-SHUKA-G + WK-KENSU.
*    欠品情報　　
     MOVE    PAR-KENSU7   TO   WK-KENSU.
     PERFORM     ZENKAKU-SEC.
     MOVE  WK-HENKAN-N  TO  MSG-G.
     COMPUTE  WK-SHUKA-G = WK-SHUKA-G + WK-KENSU.
*    出荷計
     MOVE    WK-SHUKA-X   TO   WK-KENSU.
     PERFORM     ZENKAKU-SEC.
     MOVE  WK-HENKAN-N  TO  SGOK.
*
 MEIEDT-EXIT.
     EXIT.
**********************************************************
*                    明細データ書き出し                  *
**********************************************************
 MEIWRT-SEC                   SECTION.
*
*    ADD        1          TO        IN-CNT.
**************
*帳票書き出し*
**************
     WRITE      P-REC   FROM    MEISAI-1   AFTER  5.
     WRITE      P-REC   FROM    MEISAI-2   AFTER  3.
     WRITE      P-REC   FROM    MEISAI-3   AFTER  3.
     WRITE      P-REC   FROM    MEISAI-4   AFTER  3.
     WRITE      P-REC   FROM    MEISAI-6   AFTER  6.
     WRITE      P-REC   FROM    MEISAI-7   AFTER  4.
     WRITE      P-REC   FROM    MEISAI-8   AFTER  2.
     WRITE      P-REC   FROM    MEISAI-9   AFTER  2.
     WRITE      P-REC   FROM    MEISAI-10  AFTER  2.
     WRITE      P-REC   FROM    MEISAI-11  AFTER  2.
     WRITE      P-REC   FROM    MEISAI-12  AFTER  2.
     WRITE      P-REC   FROM    MEISAI-13  AFTER  2.
     WRITE      P-REC   FROM    HASEN-2    AFTER  1.
     WRITE      P-REC   FROM    MEISAI-14  AFTER  1.
*
 MEIWRT-EXIT.
     EXIT.
****************************************************************
*    担当者マスタ検索                                          *
****************************************************************
 RD-TANMS1-SEC          SECTION.

     READ  TANMS1
       INVALID
         MOVE  1                 TO  FG-TANMS1-INV
       NOT INVALID
         MOVE  ZERO              TO  FG-TANMS1-INV
     END-READ.

 RD-TANMS1-EXIT.
     EXIT.
****************************************************************
*    倉庫マスタ検索                                          *
****************************************************************
 RD-ZSOKMS1-SEC          SECTION.

     READ  ZSOKMS1
       INVALID
         MOVE  1                 TO  FG-ZSOKMS1-INV
       NOT INVALID
         MOVE  ZERO              TO  FG-ZSOKMS1-INV
     END-READ.

 RD-ZSOKMS1-EXIT.
     EXIT.
****************************************************************
*    取引先マスタ検索                                          *
****************************************************************
 RD-TOKMS2-SEC          SECTION.

     READ  TOKMS2
       INVALID
         MOVE  1                 TO  FG-TOKMS2-INV
       NOT INVALID
         MOVE  ZERO              TO  FG-TOKMS2-INV
     END-READ.

 RD-TOKMS2-EXIT.
     EXIT.
****************************************************************
*    全角変換　　　                                          *
****************************************************************
 ZENKAKU-SEC          SECTION.
* 日本語変換する。
     MOVE   SPACE  TO  HENKAN-FLG.
     PERFORM  VARYING IX  FROM 1 BY 1 UNTIL   IX > 8
     IF ( WK-KENSU-X(IX:1) NOT = 0 ) AND
        ( WK-KENSU-X(IX:1) NOT = SPACE )
         MOVE "SET"   TO  HENKAN-FLG
     END-IF
         EVALUATE    WK-KENSU-X(IX:1)
             WHEN    0
                 IF HENKAN-FLG = "SET"
                     MOVE NC"０"  TO   WK-HENKAN-NR(IX)
                 ELSE
                     MOVE NC"　"  TO   WK-HENKAN-NR(IX)
                 END-IF
             WHEN    1
                 MOVE NC"１"  TO   WK-HENKAN-NR(IX)
             WHEN    2
                 MOVE NC"２"  TO   WK-HENKAN-NR(IX)
             WHEN    3
                 MOVE NC"３"  TO   WK-HENKAN-NR(IX)
             WHEN    4
                 MOVE NC"４"  TO   WK-HENKAN-NR(IX)
             WHEN    5
                 MOVE NC"５"  TO   WK-HENKAN-NR(IX)
             WHEN    6
                 MOVE NC"６"  TO   WK-HENKAN-NR(IX)
             WHEN    7
                 MOVE NC"７"  TO   WK-HENKAN-NR(IX)
             WHEN    8
                 MOVE NC"８"  TO   WK-HENKAN-NR(IX)
             WHEN    9
                 MOVE NC"９"  TO   WK-HENKAN-NR(IX)
         END-EVALUATE
     END-PERFORM.
 ZENKAKU-EXIT.
     EXIT.

```
