# NZT0030L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/NZT0030L.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　在庫　　　　　　　　　　　　　　  *
*    モジュール名　　　　：　在庫調整データリスト　　　　　　　*
*    作成日／作成者　　　：　2020/05/21 INOUE                  *
*    処理概要　　　　　　：　取込チェック結果をリスト出力する。*
*                            （全件・エラー分をＰＡＲＡ制御）　*
*    更新日／更新者　　　：　                                  *
*    更新内容　　　　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            NZT0030L.
*                 流用：TAB0030L.TOKSRLIB
 AUTHOR.                NAV.
 DATE-WRITTEN.          2020/05/21.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     YA        IS   YA
     YB        IS   YB
     YA-22     IS   YA-22
     YB-22     IS   YB-22
     YB-21     IS   YB-21
     YA-21     IS   YA-21
     STATION   IS   STAT
     CONSOLE   IS   CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*在庫調整累積データ
     SELECT   ZAITYOL1  ASSIGN    TO        DA-01-VI-ZAITYOL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       ZAI-F01  ZAI-F02
                                            ZAI-F030 ZAI-F031
                        FILE  STATUS   IS   ZAI-STATUS.
*プリンタ
     SELECT   PRTF      ASSIGN         LP-04-PRTF
                        FILE  STATUS   IS   PRT-STATUS.
*---------------------------------------------------------------
 DATA                   DIVISION.
 FILE                   SECTION.
*在庫調整累積データ
 FD  ZAITYOL1           LABEL RECORD   IS   STANDARD.
     COPY     ZAITYOL1  OF        XFDLIB
              JOINING   ZAI       PREFIX.
*プリンタ
 FD  PRTF               LABEL RECORD   IS   OMITTED.
 01  PRT-REC            PIC  X(200).
*
******************************************************************
 WORKING-STORAGE        SECTION.
******************************************************************
*FLG/ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  ZERO.
 01  SUTE-FLG                PIC  X(03)     VALUE  ZERO.
 01  PAGE-CNT                PIC  9(04)     VALUE  ZERO.
 01  LINE-CNT                PIC  9(02)     VALUE  ZERO.
 01  ZAITYOL1-READ-CNT       PIC  9(07)     VALUE  ZERO.
 01  SYOTANW2-READ-CNT       PIC  9(07)     VALUE  ZERO.
 01  TANMS1-INV-FLG          PIC  X(03)     VALUE  SPACE.
 01  BK-F01                  PIC  9(08)     VALUE  ZERO.
 01  BK-F02                  PIC  9(06)     VALUE  ZERO.
 01  BRK-MODE                PIC  X(03)     VALUE  SPACE.
 01  CAL-ZAI-F03A            PIC S9(11)     VALUE  ZERO.
*取込日付／時刻バックアップ
 01  WK-KEY.
     03  WK-TRDATE           PIC  9(08)     VALUE  ZERO.
     03  WK-TRTIME           PIC  9(06)     VALUE  ZERO.
*システム日付の編集
 01  WK-SYS-DATE.
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
*ステータス
 01  WK-ST.
     03  KAK-STATUS        PIC  X(02).
     03  ZAI-STATUS        PIC  X(02).
     03  SW2-STATUS        PIC  X(02).
     03  TAN-STATUS        PIC  X(02).
     03  MS1-STATUS        PIC  X(02).
     03  PL1-STATUS        PIC  X(02).
     03  PRT-STATUS        PIC  X(02).
*メッセージエリア
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "NZT0030L".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NZT0030L".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NZT0030L".
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
*--<< ﾌﾟﾘﾝﾄ AREA >>-*
 01  HD00.
     03  FILLER         CHARACTER  TYPE YB-22.
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  X(08)     VALUE  "NZT0030L".
         05  FILLER          PIC  X(25)     VALUE  SPACE.
         05  FILLER          PIC  N(16)     VALUE
                           NC"＜Ｄ３６５在庫調整データリスト＞".
     03  FILLER         CHARACTER  TYPE YA.
         05  FILLER          PIC  X(22)     VALUE  SPACE.
         05  HD00-YYYY       PIC  9(04).
         05  FILLER          PIC  N(01)     VALUE  NC"年".
         05  HD00-MM         PIC  Z9.
         05  FILLER          PIC  N(01)     VALUE  NC"月".
         05  HD00-DD         PIC  Z9.
         05  FILLER          PIC  N(01)     VALUE  NC"日".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  HD00-HH         PIC  9(02).
         05  FILLER          PIC  X(01)     VALUE  ":".
         05  HD00-SS         PIC  9(02).
         05  FILLER          PIC  X(01)     VALUE  ":".
         05  HD00-MS         PIC  9(02).
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  HD00-PCNT       PIC  ZZ9.
         05  FILLER          PIC  N(01)     VALUE  NC"頁".
*01  HD000.
*    03  FILLER         CHARACTER TYPE YA.
*        05  FILLER          PIC  X(01)     VALUE  SPACE.
*        05  FILLER          PIC  N(06)
*                            VALUE  NC"取込担当者：".
*        05  FILLER          PIC  X(01)     VALUE  SPACE.
*        05  HD000-TRBUMON   PIC  X(04).
*        05  FILLER          PIC  X(01)     VALUE  "-".
*        05  HD000-TRTANTO   PIC  X(02).
*        05  FILLER          PIC  X(01)     VALUE  SPACE.
*        05  HD000-TRTANNM   PIC  N(10).
*
 01  HD01.
     03  FILLER         CHARACTER TYPE YA.
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(05)
                             VALUE  NC"取込日付：".
*        05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  HD01-YYYY       PIC  9(04).
         05  FILLER          PIC  X(01)     VALUE  "/".
         05  HD01-MM         PIC  Z9.
         05  FILLER          PIC  X(01)     VALUE  "/".
         05  HD01-DD         PIC  Z9.
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  FILLER          PIC  N(05)
                             VALUE  NC"取込時刻：".
*        05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  HD01-HH         PIC  9(02).
         05  FILLER          PIC  X(01)     VALUE  ":".
         05  HD01-SS         PIC  9(02).
         05  FILLER          PIC  X(01)     VALUE  ":".
         05  HD01-MS         PIC  9(02).
         05  FILLER          PIC  X(15)     VALUE  SPACE.
*
     03  FILLER         CHARACTER TYPE YB.
         05  FILLER          PIC  N(06)     VALUE
             NC"エラー情報→".
*
         05  FILLER          PIC  N(08)     VALUE
             NC"_売上ＤＴ未登録".
         05  FILLER          PIC  X(02)     VALUE
               "  ".
*
         05  FILLER          PIC  N(06)     VALUE
             NC"_ＪＡＮ違い".
         05  FILLER          PIC  X(02)     VALUE
               "  ".
*
         05  FILLER          PIC  N(04)     VALUE
             NC"_計上済".
         05  FILLER          PIC  X(02)     VALUE
               "  ".
*
         05  FILLER          PIC  N(05)     VALUE
             NC"_数量以上".
         05  FILLER          PIC  X(02)     VALUE
               "  ".
*
         05  FILLER          PIC  N(07)     VALUE
             NC"_連携ＮＯ違い".
         05  FILLER          PIC  X(02)     VALUE
               "  ".
*
         05  FILLER          PIC  N(01)     VALUE
             NC"_".
         05  FILLER          PIC  X(02)     VALUE
               "  ".
*
         05  FILLER          PIC  N(01)     VALUE
             NC"_".
         05  FILLER          PIC  X(02)     VALUE
               "  ".
*
         05  FILLER          PIC  N(01)     VALUE
             NC"_".
         05  FILLER          PIC  X(02)     VALUE
               "  ".
*
         05  FILLER          PIC  N(01)     VALUE
             NC"_".
         05  FILLER          PIC  X(02)     VALUE
               "  ".
*
         05  FILLER          PIC  N(03)     VALUE
             NC"_正常".
*
 01  HD02.
     03  FILLER              PIC  X(141)    VALUE  ALL "=".
 01  HD021.
     03  FILLER              PIC  X(141)    VALUE  ALL "-".
*
 01  HD03.
     03  FILLER         CHARACTER TYPE YB.
         05  FILLER          PIC  X(111)   VALUE  SPACE.
         05  FILLER          PIC  N(10)
                             VALUE NC"　＜－－－　エラー情".
         05  FILLER          PIC  N(08)
                             VALUE NC"報　－－－　＞　".
 01  HD04.
     03  FILLER         CHARACTER TYPE YA.
         05  FILLER        PIC  X(01)  VALUE SPACE.
         05  FILLER        PIC  N(04)  VALUE NC"連携ＮＯ".
         05  FILLER        PIC  X(01)  VALUE SPACE.
         05  FILLER        PIC  N(02)  VALUE NC"倉庫".
         05  FILLER        PIC  X(02)  VALUE SPACE.
         05  FILLER        PIC  X(04)  VALUE "D365".
         05  FILLER        PIC  N(04)  VALUE NC"伝票番号".
         05  FILLER        PIC  X(09)  VALUE SPACE.
         05  FILLER        PIC  N(01)  VALUE NC"行".
         05  FILLER        PIC  X(01)  VALUE SPACE.
         05  FILLER        PIC  N(05)  VALUE NC"ＪＡＮＣＤ".
         05  FILLER        PIC  X(19)  VALUE SPACE.
         05  FILLER        PIC  N(03)  VALUE NC"数　量".
         05  FILLER        PIC  X(01)  VALUE SPACE.
         05  FILLER        PIC  N(01)  VALUE NC"（".
         05  FILLER        PIC  X(01)  VALUE SPACE.
         05  FILLER        PIC  N(03)  VALUE NC"取引先".
         05  FILLER        PIC  X(01)  VALUE "-".
         05  FILLER        PIC  X(03)  VALUE SPACE.
         05  FILLER        PIC  N(02)  VALUE NC"伝票".
         05  FILLER        PIC  X(02)  VALUE "NO".
         05  FILLER        PIC  X(01)  VALUE "-".
         05  FILLER        PIC  X(01)  VALUE SPACE.
         05  FILLER        PIC  N(02)  VALUE NC"店舗".
         05  FILLER        PIC  X(01)  VALUE "-".
         05  FILLER        PIC  X(01)  VALUE SPACE.
         05  FILLER        PIC  N(03)  VALUE NC"納品日".
         05  FILLER        PIC  N(01)  VALUE NC"）".
         05  FILLER        PIC  X(01)  VALUE SPACE.
     03  FILLER         CHARACTER TYPE YB.
         05  FILLER        PIC  N(01)  VALUE NC"_".
         05  FILLER        PIC  X(01)  VALUE SPACE.
         05  FILLER        PIC  N(01)  VALUE NC"_".
         05  FILLER        PIC  X(01)  VALUE SPACE.
         05  FILLER        PIC  N(01)  VALUE NC"_".
         05  FILLER        PIC  X(01)  VALUE SPACE.
         05  FILLER        PIC  N(01)  VALUE NC"_".
         05  FILLER        PIC  X(01)  VALUE SPACE.
         05  FILLER        PIC  N(01)  VALUE NC"_".
         05  FILLER        PIC  X(01)  VALUE SPACE.
         05  FILLER        PIC  N(01)  VALUE NC"_".
         05  FILLER        PIC  X(01)  VALUE SPACE.
         05  FILLER        PIC  N(01)  VALUE NC"_".
         05  FILLER        PIC  X(01)  VALUE SPACE.
         05  FILLER        PIC  N(01)  VALUE NC"_".
         05  FILLER        PIC  X(01)  VALUE SPACE.
         05  FILLER        PIC  N(01)  VALUE NC"_".
         05  FILLER        PIC  X(01)  VALUE SPACE.
         05  FILLER        PIC  N(01)  VALUE NC"_".
*                            VALUE NC"__________"
*        05  FILLER        PIC  N(10)
*                            VALUE NC"_　_　_　_　_　"
*        05  FILLER        PIC  N(10)
*                            VALUE NC"_　_　_　_　_　".
*
*01  HD045.
*    03  FILLER       CHARACTER TYPE YB.
*        05  FILLER   PIC  X(02) VALUE SPACE.
*        05  FILLER   PIC  N(08) VALUE NC"Ｄ３６５商品ＣＤ".
*        05  FILLER   PIC  X(12) VALUE SPACE.
*        05  FILLER   PIC  N(10) VALUE NC"Ｄ３６５ＪＡＮＣＤ　".
*        05  FILLER   PIC  X(15) VALUE SPACE.
*        05  FILLER   PIC  N(02) VALUE NC"区分".
*        05  FILLER   PIC  X(02) VALUE "( ".
*        05  FILLER   PIC  N(02) VALUE NC"定番".
*        05  FILLER   PIC  X(01) VALUE " ".
*        05  FILLER   PIC  N(02) VALUE NC"分類".
*        05  FILLER   PIC  X(01) VALUE " ".
*        05  FILLER   PIC  X(02) VALUE "20".
*        05  FILLER   PIC  N(02) VALUE NC"分類".
*        05  FILLER   PIC  X(01) VALUE " ".
*        05  FILLER   PIC  N(04) VALUE NC"物流束　".
*        05  FILLER   PIC  N(02) VALUE NC"廃盤".
*        05  FILLER   PIC  X(01) VALUE " ".
*        05  FILLER   PIC  N(02) VALUE NC"管理".
*        05  FILLER   PIC  X(01) VALUE " ".
*        05  FILLER   PIC  N(02) VALUE NC"小売".
*        05  FILLER   PIC  X(01) VALUE " ".
*        05  FILLER   PIC  N(02) VALUE NC"振替".
*        05  FILLER   PIC  X(02) VALUE " )".
*01  HD05.
*    03  FILLER         CHARACTER TYPE YB.
*        05  FILLER     PIC  X(02) VALUE SPACE.
*        05  FILLER     PIC  N(03) VALUE NC"商品名".
*
 01  MS01.
   03  FILLER              CHARACTER TYPE YB.
     05  FILLER              PIC  X(01)     VALUE  SPACE.
     05  MS01-RENKEINO       PIC  X(09).
     05  FILLER              PIC  X(01)     VALUE  SPACE.
     05  MS01-SOKCD          PIC  X(03).
     05  FILLER              PIC  X(01)     VALUE  SPACE.
     05  MS01-D365DENNO      PIC  X(20).
     05  FILLER              PIC  X(01)     VALUE  SPACE.
     05  MS01-D365GYONO      PIC  ZZ.
     05  FILLER              PIC  X(01)     VALUE  SPACE.
     05  MS01-JANCD          PIC  X(20).
     05  FILLER              PIC  X(02)     VALUE  SPACE.
     05  MS01-SURYO          PIC  --,---,---,--9.
     05  FILLER              PIC  X(01)     VALUE  SPACE.
     05  FILLER              PIC  X(01)     VALUE  "(".
     05  MS01-TORICD         PIC  X(08).
     05  FILLER              PIC  X(01)     VALUE  "-".
     05  MS01-DENNO          PIC  X(09).
     05  FILLER              PIC  X(01)     VALUE  "-".
     05  MS01-TENCD          PIC  X(05).
     05  FILLER              PIC  X(01)     VALUE  "-".
     05  MS01-NOUHINBI       PIC  X(08).
     05  FILLER              PIC  X(01)     VALUE  ")".
     05  FILLER              PIC  X(01)     VALUE  SPACE.
     05  FILLER              CHARACTER TYPE YB.
         07  MS01-ERR1       PIC  N(01).
         07  FILLER          PIC  X(01)     VALUE  SPACE.
         07  MS01-ERR2       PIC  N(01).
         07  FILLER          PIC  X(01)     VALUE  SPACE.
         07  MS01-ERR3       PIC  N(01).
         07  FILLER          PIC  X(01)     VALUE  SPACE.
         07  MS01-ERR4       PIC  N(01).
         07  FILLER          PIC  X(01)     VALUE  SPACE.
         07  MS01-ERR5       PIC  N(01).
         07  FILLER          PIC  X(01)     VALUE  SPACE.
         07  MS01-ERR6       PIC  N(01).
         07  FILLER          PIC  X(01)     VALUE  SPACE.
         07  MS01-ERR7       PIC  N(01).
         07  FILLER          PIC  X(01)     VALUE  SPACE.
         07  MS01-ERR8       PIC  N(01).
         07  FILLER          PIC  X(01)     VALUE  SPACE.
         07  MS01-ERR9       PIC  N(01).
         07  FILLER          PIC  X(01)     VALUE  SPACE.
         07  MS01-ERR10      PIC  N(01).
*01  MS02.
*    03  FILLER              CHARACTER TYPE YB.
*        05  FILLER          PIC  X(02)     VALUE  SPACE.
*        05  MS02-D365SHOCD  PIC  X(20).
*        05  FILLER          PIC  X(04)     VALUE  SPACE.
*        05  MS02-D365JANCD  PIC  X(20).
*        05  FILLER          PIC  X(15)     VALUE  SPACE.
*        05  FILLER          PIC  X(01)     VALUE  "(".
*        05  MS02-KBN-TEIBAN PIC  X(01).
*        05  FILLER          PIC  X(01)     VALUE  ")".
*        05  FILLER          PIC  X(01)     VALUE  SPACE.
*        05  FILLER          PIC  X(01)     VALUE  "(".
*        05  MS02-KBN-BUNRUI PIC  X(01).
*        05  FILLER          PIC  X(01)     VALUE  ")".
*        05  FILLER          PIC  X(02)     VALUE  SPACE.
*        05  FILLER          PIC  X(01)     VALUE  "(".
*        05  MS02-KBN-20BUNR PIC  X(02).
*        05  FILLER          PIC  X(01)     VALUE  ")".
*        05  FILLER          PIC  X(02)     VALUE  SPACE.
*        05  FILLER          PIC  X(01)     VALUE  "(".
*        05  MS02-KBN-TABA   PIC  X(01).
*        05  FILLER          PIC  X(01)     VALUE  ")".
*        05  FILLER          PIC  X(02)     VALUE  SPACE.
*        05  FILLER          PIC  X(01)     VALUE  "(".
*        05  MS02-KBN-HAIBAN PIC  X(01).
*        05  FILLER          PIC  X(01)     VALUE  ")".
*        05  FILLER          PIC  X(01)     VALUE  SPACE.
*        05  FILLER          PIC  X(01)     VALUE  "(".
*        05  MS02-KBN-KANRI  PIC  X(01).
*        05  FILLER          PIC  X(01)     VALUE  ")".
*        05  FILLER          PIC  X(01)     VALUE  SPACE.
*        05  FILLER          PIC  X(01)     VALUE  "(".
*        05  MS02-KBN-KOURI  PIC  X(01).
*        05  FILLER          PIC  X(01)     VALUE  ")".
*        05  FILLER          PIC  X(01)     VALUE  SPACE.
*        05  FILLER          PIC  X(01)     VALUE  "(".
*        05  MS02-KBN-FURI   PIC  X(01).
*        05  FILLER          PIC  X(01)     VALUE  ")".
*01  MS03.
*    03  FILLER              CHARACTER TYPE YB.
*        05  FILLER          PIC  X(02)     VALUE  SPACE.
*        05  MS03-SHONM1     PIC  N(15).
*        05  MS03-SHONM2     PIC  N(15).
*        05  FILLER          PIC  X(04)     VALUE  SPACE.
*        05  MS03-SHOKANA1   PIC  X(15).
*        05  MS03-SHOKANA2   PIC  X(15).
*
*検印欄１
 01  KEN01.
     05  FILLER              PIC  X(117) VALUE SPACE.
     05  FILLER              PIC  X(19)  VALUE
         "+-----+-----+-----+".
*検印欄２
 01  KEN02.
     05  FILLER              PIC  X(117) VALUE SPACE.
     05  FILLER              PIC  X(19)  VALUE
         "!     !     !     !".
*
*対象データなし
 01  LST-DATA-X.
     03  FILLER         CHARACTER TYPE YB-21.
         05  FILLER          PIC  X(25)     VALUE  SPACE.
         05  FILLER          PIC  N(22)     VALUE
             NC"＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃".
 01  LST-DATA-Y.
     03  FILLER         CHARACTER TYPE YB-21.
         05  FILLER          PIC  X(25)     VALUE  SPACE.
         05  FILLER          PIC  N(22)     VALUE
             NC"＃　　　　取込データはありません。　　　　＃".
 01  LST-DATA-Z.
     03  FILLER         CHARACTER TYPE YB-21.
         05  FILLER          PIC  X(25)     VALUE  SPACE.
         05  FILLER          PIC  N(22)     VALUE
             NC"＃　　　　　　　　　　　　　　　　　　　　＃".
*
 01  P-SPACE            PIC  X(01)     VALUE  SPACE.
 01  P-LINE1            PIC  X(141)    VALUE  ALL   "-".
 01  P-LINE2            PIC  X(141)    VALUE  ALL   "=".
*時刻編集
 01  SYS-TIME                PIC  9(08).
 01  WK-TIME      REDEFINES  SYS-TIME.
   03  WK-TIME-HM            PIC  9(06).
   03  WK-TIME-FIL           PIC  X(02).
*日付サブルーチン用
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
 LINKAGE                SECTION.
 01  LINK-IN-SDATE          PIC   9(08).
 01  LINK-IN-EDATE          PIC   9(08).
 01  LINK-IN-STIME          PIC   9(06).
 01  LINK-IN-ETIME          PIC   9(06).
 01  LINK-IN-KUBUN          PIC   X(01).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE       DIVISION  USING
                                     LINK-IN-SDATE
                                     LINK-IN-EDATE
                                     LINK-IN-STIME
                                     LINK-IN-ETIME
                                     LINK-IN-KUBUN.
 DECLARATIVES.
*
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   ZAITYOL1.
     MOVE      "ZAITYOL1"   TO   AB-FILE.
     MOVE      ZAI-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC4           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   PRTF.
     MOVE      "PRTF    "   TO   AB-FILE.
     MOVE      PRT-STATUS   TO   AB-STS.
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
*ファイルＯＰＥＮ
     OPEN     INPUT  ZAITYOL1.
     OPEN     OUTPUT PRTF.
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
     ACCEPT   SYS-TIME          FROM   TIME.
*
*累積ファイルスタート
     PERFORM  ZAITYOL1-START-SEC.
     IF  END-FLG = "END"
         DISPLAY NC"＃＃　取込データ　なし　１＃＃" UPON CONS
         PERFORM  HEAD-WT-SEC
         WRITE    PRT-REC      FROM  LST-DATA-X  AFTER  5
         WRITE    PRT-REC      FROM  LST-DATA-Z  AFTER  1
         WRITE    PRT-REC      FROM  LST-DATA-Y  AFTER  1
         WRITE    PRT-REC      FROM  LST-DATA-Z  AFTER  1
         WRITE    PRT-REC      FROM  LST-DATA-X  AFTER  1
         MOVE    "END"         TO    END-FLG
         GO                    TO    INIT-EXIT
     END-IF.
*
*累積ファイル読込
     PERFORM ZAITYOL1-READ-SEC.
     IF  END-FLG = "END"
         DISPLAY NC"＃＃　取込データ　なし　２＃＃"  UPON CONS
         PERFORM  HEAD-WT-SEC
         WRITE    PRT-REC      FROM  LST-DATA-X  AFTER  5
         WRITE    PRT-REC      FROM  LST-DATA-Z  AFTER  1
         WRITE    PRT-REC      FROM  LST-DATA-Y  AFTER  1
         WRITE    PRT-REC      FROM  LST-DATA-Z  AFTER  1
         WRITE    PRT-REC      FROM  LST-DATA-X  AFTER  1
         MOVE    "END"         TO    END-FLG
         GO                    TO    INIT-EXIT
     END-IF.
*
     MOVE         SPACE        TO    BRK-MODE.
*
*ヘッダ印刷
     PERFORM HEAD-WT-SEC.
*
 INIT-EXIT.
     EXIT.
*
****************************************************************
* 累積ファイル  スタート
****************************************************************
 ZAITYOL1-START-SEC          SECTION.
*
     MOVE    "ZAITYOL1-START-SEC" TO   S-NAME.
*
     MOVE     SPACE               TO   ZAI-REC.
     INITIALIZE                        ZAI-REC.
*
     MOVE     LINK-IN-SDATE       TO   ZAI-F01.
     MOVE     LINK-IN-STIME       TO   ZAI-F02.
     MOVE     SPACE               TO   ZAI-F030.
     MOVE     ZERO                TO   ZAI-F031.
*
     START  ZAITYOL1  KEY  IS  >=      ZAI-F01  ZAI-F02
                                       ZAI-F030 ZAI-F031
            INVALID
            MOVE    "END"         TO   END-FLG
     END-START.
*
 ZAITYOL1-START-EXIT.
     EXIT.
*
****************************************************************
* 累積ファイル    読込
****************************************************************
 ZAITYOL1-READ-SEC           SECTION.
*
     MOVE    "ZAITYOL1-READ-SEC"   TO   S-NAME.
*
     READ     ZAITYOL1  AT  END
              MOVE     "END"      TO   END-FLG
*             DISPLAY "AAA" UPON CONS
              GO                  TO   ZAITYOL1-READ-EXIT
     END-READ.
 ZAITYOL1-READ-010.
*対象判定
     IF       LINK-IN-KUBUN =  " "
              CONTINUE
     ELSE
              IF   ZAI-F04  =  " "
                   GO       TO         ZAITYOL1-READ-SEC
              END-IF
     END-IF.
 ZAITYOL1-READ-020.
*    IF     ( LINK-IN-SDATE > ZAI-F01 ) OR
*           ( LINK-IN-EDATE < ZAI-F01 )
*             MOVE     "END"      TO   END-FLG
*             GO                  TO   ZAITYOL1-READ-EXIT
*    END-IF.
*    IF     ( LINK-IN-STIME > ZAI-F02 ) OR
*           ( LINK-IN-ETIME < ZAI-F02 )
*             MOVE     "END"      TO   END-FLG
*             GO                  TO   ZAITYOL1-READ-EXIT
*    END-IF.
     IF       LINK-IN-EDATE < ZAI-F01
              MOVE     "END"      TO   END-FLG
              GO                  TO   ZAITYOL1-READ-EXIT
     END-IF.
*
 ZAITYOL1-READ-030.
     IF    (  LINK-IN-STIME > ZAI-F02 ) OR
           (  LINK-IN-ETIME < ZAI-F02 )
              GO                  TO   ZAITYOL1-READ-SEC
     END-IF.
*
 ZAITYOL1-READ-040.
*モードブレイク判定
     IF     ( ZAI-F01   NOT =  BK-F01 ) OR
            ( ZAI-F02   NOT =  BK-F02 )
              MOVE     "BRK"      TO   BRK-MODE
              MOVE      ZAI-F01   TO   BK-F01
              MOVE      ZAI-F02   TO   BK-F02
     ELSE
              MOVE     "   "      TO   BRK-MODE
     END-IF.
*
*件数カウント
     ADD      1                   TO   ZAITYOL1-READ-CNT.
*
 ZAITYOL1-READ-EXIT.
     EXIT.
*
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"          TO   S-NAME.
*
 MAIN-010.
*印刷処理
     PERFORM  MEISAI-WT-SEC.
     MOVE     SPACE  TO   PRT-REC.
*
 MAIN-020.
     PERFORM  ZAITYOL1-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
*
*--------------------------------------------------------------*
*    ヘッダ印字
*--------------------------------------------------------------*
 HEAD-WT-SEC            SECTION.
     MOVE    "HEAD-WT-SEC"        TO   S-NAME.
*    改頁判定
     IF       PAGE-CNT  >   ZERO
              MOVE      SPACE     TO   PRT-REC
              WRITE     PRT-REC   AFTER   PAGE
     END-IF.
*    行カウンター初期化
     MOVE     ZERO                TO   LINE-CNT.
*    頁カウンター
     ADD      1                   TO   PAGE-CNT.
     MOVE     PAGE-CNT            TO   HD00-PCNT.
*    システム日付セット
     MOVE     SYS-DATEW(1:4)      TO   HD00-YYYY.
     MOVE     SYS-DATEW(5:2)      TO   HD00-MM.
     MOVE     SYS-DATEW(7:2)      TO   HD00-DD.
*    システム時刻セット
     MOVE     WK-TIME-HM(1:2)     TO   HD00-HH.
     MOVE     WK-TIME-HM(3:2)     TO   HD00-SS.
     MOVE     WK-TIME-HM(5:2)     TO   HD00-MS.
*    取込日付セット
     MOVE     ZAI-F01(1:4)        TO   HD01-YYYY.
     MOVE     ZAI-F01(5:2)        TO   HD01-MM.
     MOVE     ZAI-F01(7:2)        TO   HD01-DD.
*    時刻セット
     MOVE     ZAI-F02(1:2)        TO   HD01-HH.
     MOVE     ZAI-F02(3:2)        TO   HD01-SS.
     MOVE     ZAI-F02(5:2)        TO   HD01-MS.
*    ヘッダ印刷
*
*****WRITE    PRT-REC       FROM  KEN01 AFTER  1.
*****WRITE    PRT-REC       FROM  KEN02 AFTER  1.
*****WRITE    PRT-REC       FROM  KEN02 AFTER  1.
*****WRITE    PRT-REC       FROM  KEN02 AFTER  1.
*****WRITE    PRT-REC       FROM  KEN01 AFTER  1.
*
     WRITE    PRT-REC       FROM  HD00  AFTER  2.
*****WRITE    PRT-REC       FROM  HD000 AFTER  2.
     WRITE    PRT-REC       FROM  HD01  AFTER  2.
     WRITE    PRT-REC       FROM  HD02  AFTER  1.
     WRITE    PRT-REC       FROM  HD03  AFTER  1.
     WRITE    PRT-REC       FROM  HD04  AFTER  1.
*    WRITE    PRT-REC       FROM  HD045 AFTER  1.
*    WRITE    PRT-REC       FROM  HD05  AFTER  1.
     WRITE    PRT-REC       FROM  HD02  AFTER  1.
     MOVE     SPACE               TO    PRT-REC.
     WRITE    PRT-REC                   AFTER  1.
*行カウントアップ
*    MOVE     15                  TO    LINE-CNT.
     MOVE     11                  TO    LINE-CNT.
*
 HEAD-WT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    明細印字
*--------------------------------------------------------------*
 MEISAI-WT-SEC          SECTION.
     MOVE    "MEISAI-WT-SEC"      TO   S-NAME.
*   改頁判定
*****IF     ( LINE-CNT  >   48  ) OR
     IF     ( LINE-CNT  >   50  ) OR
            ( BRK-MODE = "BRK" )
              PERFORM HEAD-WT-SEC
     END-IF.
*   連携ＮＯ　
     MOVE      ZAI-F032        TO   MS01-RENKEINO.
*   倉庫ＣＤ　
     MOVE      ZAI-F037        TO   MS01-SOKCD.
*   D365伝票番号
     MOVE      ZAI-F030        TO   MS01-D365DENNO.
*   D365行番号
     MOVE      ZAI-F031        TO   MS01-D365GYONO.
*   ＪＡＮコード
     MOVE      ZAI-F036        TO   MS01-JANCD.
*   数量
     COMPUTE   CAL-ZAI-F03A     =   ZAI-F03A  /   100.
     IF        ZAI-F039        =  "-"
               COMPUTE  CAL-ZAI-F03A     =   CAL-ZAI-F03A   *   -1
     END-IF.
     MOVE      CAL-ZAI-F03A    TO   MS01-SURYO.
*   取引先
     MOVE      ZAI-F06         TO   MS01-TORICD.
     IF        ZAI-F05(01)  =  "1"
               MOVE    SPACE   TO   MS01-TORICD
     END-IF.
*   伝票ＮＯ
     MOVE      ZAI-F07         TO   MS01-DENNO.
     IF        ZAI-F05(01)  =  "1"
               MOVE    SPACE   TO   MS01-DENNO
     END-IF.
*   店舗ＣＤ
     MOVE      ZAI-F10         TO   MS01-TENCD.
     IF        ZAI-F05(01)  =  "1"
               MOVE    SPACE   TO   MS01-TENCD
     END-IF.
*   納品日
     MOVE      ZAI-F11         TO   MS01-NOUHINBI.
     IF        ZAI-F05(01)  =  "1"
               MOVE    SPACE   TO   MS01-NOUHINBI
     END-IF.
*
*   エラー区分1
     IF    ZAI-F05(01) = " "
           MOVE SPACE        TO   MS01-ERR1
     ELSE
           MOVE NC"×"       TO   MS01-ERR1
     END-IF.
*   エラー区分2
     IF    ZAI-F05(02) = " "
           MOVE SPACE        TO   MS01-ERR2
     ELSE
           MOVE NC"×"       TO   MS01-ERR2
     END-IF.
*   エラー区分3
     IF    ZAI-F05(03) = " "
           MOVE SPACE        TO   MS01-ERR3
     ELSE
           MOVE NC"×"       TO   MS01-ERR3
     END-IF.
*   エラー区分4
     IF    ZAI-F05(04) = " "
           MOVE SPACE        TO   MS01-ERR4
     ELSE
           MOVE NC"×"       TO   MS01-ERR4
     END-IF.
*   エラー区分5
     IF    ZAI-F05(05) = " "
           MOVE SPACE        TO   MS01-ERR5
     ELSE
           MOVE NC"×"       TO   MS01-ERR5
     END-IF.
*   エラー区分6
     IF    ZAI-F05(06) = " "
           MOVE SPACE        TO   MS01-ERR6
     ELSE
           MOVE NC"×"       TO   MS01-ERR6
     END-IF.
*   エラー区分7
     IF    ZAI-F05(07) = " "
           MOVE SPACE        TO   MS01-ERR7
     ELSE
           MOVE NC"×"       TO   MS01-ERR7
     END-IF.
*   エラー区分8
     IF    ZAI-F05(08) = " "
           MOVE SPACE        TO   MS01-ERR8
     ELSE
           MOVE NC"×"       TO   MS01-ERR8
     END-IF.
*   エラー区分9
     IF    ZAI-F05(09) = " "
           MOVE SPACE        TO   MS01-ERR9
     ELSE
           MOVE NC"×"       TO   MS01-ERR9
     END-IF.
*    エラー区分１０("1"＝エラー無し)
     IF    ZAI-F05(10) = " "
           MOVE SPACE        TO   MS01-ERR10
     ELSE
           MOVE NC"○"       TO   MS01-ERR10
     END-IF.
*    明細印刷
     WRITE    PRT-REC       FROM  MS01  AFTER  1.
*    WRITE    PRT-REC       FROM  MS02  AFTER  1.
*    WRITE    PRT-REC       FROM  MS03  AFTER  1.
*****WRITE    PRT-REC       FROM  HD021 AFTER  1.
*行カウント
*****ADD      2                   TO    LINE-CNT.
     ADD      1                   TO    LINE-CNT.
*
 MEISAI-WT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*件数表示
     DISPLAY "ZAITYOL1 PRINT-CNT = " ZAITYOL1-READ-CNT UPON CONS.
*
     CLOSE    ZAITYOL1   PRTF.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
