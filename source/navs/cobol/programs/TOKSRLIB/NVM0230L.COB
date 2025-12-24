****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　マスタ保守　　　　　　　　　　　  *
*    モジュール名　　　　：（ＥＸＣＥＬ取込）　　　　　　　　　*
*    　　　　　　　　　　　　商品名称マスタ　取込チェックリスト*
*    作成日／作成者　　　：　2020/04/15 INOUE                  *
*    処理概要　　　　　　：　取込チェック結果をリスト出力する。*
*                            （全件・エラー分をＰＡＲＡ制御）　*
*    流用元　　　　　　　：　NVM0130L.TOKSRLIB                 *
*    更新日／更新者　　　：　2020/05/28 INOUE                  *
*    更新内容　　　　　　　　オーダー区分追加　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            NVM0230L.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2020/04/15.
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
*商品名称マスタ　取込ファイル3
     SELECT   SMSMNTW3  ASSIGN    TO        DA-01-VI-SMSMNTW3
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       SW3-F11  SW3-F13
                                            SW3-F14
                        FILE  STATUS   IS   SW3-STATUS.
*商品名称マスタ　取込ファイル2
     SELECT   SMSMNTW2  ASSIGN    TO        DA-01-VI-SMSMNTW2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       SW2-F060 SW2-F11
                                            SW2-F13  SW2-F14
                        FILE  STATUS   IS   SW2-STATUS.
*担当者マスタ
     SELECT     TANMS1       ASSIGN    TO       DA-01-VI-TANMS1
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     RANDOM
                             RECORD    KEY      TAN-F01  TAN-F02
                             FILE      STATUS   TAN-STATUS.
*プリンタ
     SELECT   PRTF      ASSIGN         LP-04-PRTF
                        FILE  STATUS   IS   PRT-STATUS.
*---------------------------------------------------------------
 DATA                   DIVISION.
 FILE                   SECTION.
*商品名称マスタ　取込ファイル1
 FD  SMSMNTW3           LABEL RECORD   IS   STANDARD.
     COPY     SMSMNTW3  OF        XFDLIB
              JOINING   SW3       PREFIX.
*商品名称マスタ　取込ファイル2
 FD  SMSMNTW2           LABEL RECORD   IS   STANDARD.
     COPY     SMSMNTW2  OF        XFDLIB
              JOINING   SW2       PREFIX.
*担当者マスタ
 FD  TANMS1             LABEL RECORD   IS   STANDARD.
     COPY     TANMS1    OF        XFDLIB
     JOINING  TAN       AS        PREFIX.
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
 01  SMSMNTW3-READ-CNT       PIC  9(07)     VALUE  ZERO.
 01  SMSMNTW2-READ-CNT       PIC  9(07)     VALUE  ZERO.
 01  TANMS1-INV-FLG          PIC  X(03)     VALUE  SPACE.
 01  BK-F11                  PIC  X(01)     VALUE  SPACE.
 01  BRK-MODE                PIC  X(03)     VALUE  SPACE.
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
     03  SW3-STATUS        PIC  X(02).
     03  SW2-STATUS        PIC  X(02).
     03  TAN-STATUS        PIC  X(02).
     03  MS1-STATUS        PIC  X(02).
     03  PL1-STATUS        PIC  X(02).
     03  PRT-STATUS        PIC  X(02).
*メッセージエリア
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "NVM0230L".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NVM0230L".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NVM0230L".
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
         05  FILLER          PIC  X(08)     VALUE  "NVM0230L".
         05  FILLER          PIC  X(05)     VALUE  SPACE.
         05  HD00-MODE       PIC  N(05).
         05  FILLER          PIC  X(03)     VALUE  SPACE.
         05  FILLER          PIC  N(11)     VALUE
                             NC"＜　商品名称マスタ　　".
         05  HD00-OUTSYUN    PIC  N(11).
     03  FILLER         CHARACTER  TYPE YA.
         05  FILLER          PIC  X(10)     VALUE  SPACE.
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
 01  HD000.
     03  FILLER         CHARACTER TYPE YA.
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(06)
                             VALUE  NC"取込担当者：".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  HD000-TRBUMON   PIC  X(04).
         05  FILLER          PIC  X(01)     VALUE  "-".
         05  HD000-TRTANTO   PIC  X(02).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  HD000-TRTANNM   PIC  N(10).
*
 01  HD01.
     03  FILLER         CHARACTER TYPE YA.
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(06)
                             VALUE  NC"取込日付　：".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  HD01-YYYY       PIC  9(04).
         05  FILLER          PIC  X(01)     VALUE  "/".
         05  HD01-MM         PIC  Z9.
         05  FILLER          PIC  X(01)     VALUE  "/".
         05  HD01-DD         PIC  Z9.
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(05)
                             VALUE  NC"取込時刻：".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
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
         05  FILLER          PIC  N(03)     VALUE
             NC"_名称".
         05  FILLER          PIC  X(02)     VALUE
               "M ".
*
         05  FILLER          PIC  N(04)     VALUE
             NC"_仕入先".
         05  FILLER          PIC  X(02)     VALUE
               "M ".
*
         05  FILLER          PIC  N(03)     VALUE
             NC"_区分".
         05  FILLER          PIC  X(02)     VALUE
               "  ".
*
         05  FILLER          PIC  N(05)     VALUE
             NC"_必須項目".
         05  FILLER          PIC  X(02)     VALUE
               "  ".
*
         05  FILLER          PIC  N(01)     VALUE
             NC"_".
         05  FILLER          PIC  N(03)     VALUE
             NC"Ｄ商品".
         05  FILLER          PIC  X(04)     VALUE
               "CD  ".
*
         05  FILLER          PIC  N(01)     VALUE
             NC"_".
         05  FILLER          PIC  N(04)     VALUE
             NC"ＤＪＡＮ".
         05  FILLER          PIC  X(04)     VALUE
               "CD  ".
*
         05  FILLER          PIC  N(05)     VALUE
             NC"_整合性　".
*
         05  FILLER          PIC  N(02)     VALUE
             NC"_　".
*
         05  FILLER          PIC  N(02)     VALUE
             NC"_　".
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
         05  FILLER          PIC  X(110)   VALUE  SPACE.
         05  FILLER          PIC  N(10)
                             VALUE NC"　＜－－－　エラー情".
         05  FILLER          PIC  N(08)
                             VALUE NC"報　－－－　＞　".
 01  HD04.
     03  FILLER         CHARACTER TYPE YB.
*        05  FILLER        PIC  X(02)  VALUE SPACE.
         05  FILLER        PIC  N(04)  VALUE NC"商品ＣＤ".
         05  FILLER        PIC  X(06)  VALUE SPACE.
         05  FILLER        PIC  N(04)  VALUE NC"品単ＣＤ".
         05  FILLER        PIC  X(08)  VALUE SPACE.
         05  FILLER        PIC  N(06)  VALUE NC"ＪＡＮＣＤ　".
         05  FILLER        PIC  X(07)  VALUE SPACE.
         05  FILLER        PIC  N(04)  VALUE NC"主仕入先".
         05  FILLER        PIC  X(08)  VALUE SPACE.
         05  FILLER        PIC  N(02)  VALUE NC"入数".
         05  FILLER        PIC  X(09)  VALUE SPACE.
         05  FILLER        PIC  N(04)  VALUE NC"仕入単価".
         05  FILLER        PIC  X(08)  VALUE SPACE.
         05  FILLER        PIC  N(04)  VALUE NC"基準原価".
         05  FILLER        PIC  X(08)  VALUE SPACE.
         05  FILLER        PIC  N(04)  VALUE NC"基準売価".
         05  FILLER        PIC  X(09)  VALUE SPACE.
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
 01  HD045.
     03  FILLER       CHARACTER TYPE YB.
         05  FILLER   PIC  X(02) VALUE SPACE.
         05  FILLER   PIC  N(08) VALUE NC"Ｄ３６５商品ＣＤ".
         05  FILLER   PIC  X(12) VALUE SPACE.
         05  FILLER   PIC  N(10) VALUE NC"Ｄ３６５ＪＡＮＣＤ　".
         05  FILLER   PIC  X(15) VALUE SPACE.
         05  FILLER   PIC  N(02) VALUE NC"区分".
         05  FILLER   PIC  X(02) VALUE "( ".
         05  FILLER   PIC  N(02) VALUE NC"定番".
         05  FILLER   PIC  X(01) VALUE " ".
         05  FILLER   PIC  N(02) VALUE NC"分類".
         05  FILLER   PIC  X(01) VALUE " ".
         05  FILLER   PIC  X(02) VALUE "20".
         05  FILLER   PIC  N(02) VALUE NC"分類".
         05  FILLER   PIC  X(01) VALUE " ".
         05  FILLER   PIC  N(04) VALUE NC"物流束　".
         05  FILLER   PIC  N(02) VALUE NC"廃盤".
         05  FILLER   PIC  X(01) VALUE " ".
         05  FILLER   PIC  N(02) VALUE NC"管理".
         05  FILLER   PIC  X(01) VALUE " ".
         05  FILLER   PIC  N(02) VALUE NC"小売".
         05  FILLER   PIC  X(01) VALUE " ".
         05  FILLER   PIC  N(02) VALUE NC"振替".
         05  FILLER   PIC  X(02) VALUE " )".
 01  HD05.
     03  FILLER         CHARACTER TYPE YB.
         05  FILLER     PIC  X(02) VALUE SPACE.
         05  FILLER     PIC  N(03) VALUE NC"商品名".
*2020.05.28↓
         05  FILLER     PIC  X(79) VALUE SPACE.
         05  FILLER     PIC  N(06) VALUE NC"オーダー区分".
*2020.05.28↑
*
 01  MS01.
*    03  FILLER              CHARACTER TYPE YB.
     03  FILLER              PIC  X(02)     VALUE  "( ".
     03  MS01-SHOCD          PIC  X(08).
     03  FILLER              PIC  X(02)     VALUE  SPACE.
     03  MS01-HINTAN         PIC  X(10).
     03  FILLER              PIC  X(02)     VALUE  " )".
     03  FILLER              PIC  X(02)     VALUE  SPACE.
     03  MS01-JANCD          PIC  X(13).
     03  FILLER              PIC  X(02)     VALUE  SPACE.
     03  MS01-SHICD          PIC  X(08).
     03  FILLER              PIC  X(06)     VALUE  SPACE.
     03  MS01-IRISU          PIC  X(04).
     03  MS01-IRISUR         REDEFINES  MS01-IRISU.
         05 MS01-IRISU4      PIC  ZZZ9.
     03  FILLER              PIC  X(03)     VALUE  SPACE.
     03  MS01-STANKA         PIC  X(12).
     03  MS01-STANKAR        REDEFINES  MS01-STANKA.
         05 MS01-STANKA9     PIC  Z,ZZZ,ZZ9.99.
     03  FILLER              PIC  X(02)     VALUE  SPACE.
     03  MS01-KGENKA         PIC  X(12).
     03  MS01-KGENKAR        REDEFINES  MS01-KGENKA.
         05 MS01-KGENKA9     PIC  Z,ZZZ,ZZ9.99.
     03  FILLER              PIC  X(02)     VALUE  SPACE.
     03  MS01-KBAIKA         PIC  X(12).
     03  MS01-KBAIKAR        REDEFINES  MS01-KBAIKA.
         05 MS01-KBAIKA9     PIC  Z,ZZZ,ZZ9.99.
     03  FILLER              PIC  X(09)     VALUE  SPACE.
     03  FILLER              CHARACTER TYPE YB.
         05  MS01-ERR1       PIC  N(01).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-ERR2       PIC  N(01).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-ERR3       PIC  N(01).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-ERR4       PIC  N(01).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-ERR5       PIC  N(01).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-ERR6       PIC  N(01).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-ERR7       PIC  N(01).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-ERR8       PIC  N(01).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-ERR9       PIC  N(01).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-ERR10      PIC  N(01).
 01  MS02.
*    03  FILLER              CHARACTER TYPE YB.
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  MS02-D365SHOCD  PIC  X(20).
         05  FILLER          PIC  X(04)     VALUE  SPACE.
         05  MS02-D365JANCD  PIC  X(20).
         05  FILLER          PIC  X(15)     VALUE  SPACE.
         05  FILLER          PIC  X(01)     VALUE  "(".
         05  MS02-KBN-TEIBAN PIC  X(01).
         05  FILLER          PIC  X(01)     VALUE  ")".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  X(01)     VALUE  "(".
         05  MS02-KBN-BUNRUI PIC  X(01).
         05  FILLER          PIC  X(01)     VALUE  ")".
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  FILLER          PIC  X(01)     VALUE  "(".
         05  MS02-KBN-20BUNR PIC  X(02).
         05  FILLER          PIC  X(01)     VALUE  ")".
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  FILLER          PIC  X(01)     VALUE  "(".
         05  MS02-KBN-TABA   PIC  X(01).
         05  FILLER          PIC  X(01)     VALUE  ")".
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  FILLER          PIC  X(01)     VALUE  "(".
         05  MS02-KBN-HAIBAN PIC  X(01).
         05  FILLER          PIC  X(01)     VALUE  ")".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  X(01)     VALUE  "(".
         05  MS02-KBN-KANRI  PIC  X(01).
         05  FILLER          PIC  X(01)     VALUE  ")".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  X(01)     VALUE  "(".
         05  MS02-KBN-KOURI  PIC  X(01).
         05  FILLER          PIC  X(01)     VALUE  ")".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  X(01)     VALUE  "(".
         05  MS02-KBN-FURI   PIC  X(01).
         05  FILLER          PIC  X(01)     VALUE  ")".
 01  MS03.
     03  FILLER              CHARACTER TYPE YB.
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  MS03-SHONM1     PIC  N(15).
         05  MS03-SHONM2     PIC  N(15).
         05  FILLER          PIC  X(04)     VALUE  SPACE.
         05  MS03-SHOKANA1   PIC  X(15).
         05  MS03-SHOKANA2   PIC  X(15).
*2020.05.28↓
         05  FILLER          PIC  X(05)     VALUE  SPACE.
         05  MS03-ORDER      PIC  X(10).
*2020.05.28↑
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
 01  PARA-IN-BUMON         PIC   X(04).
 01  PARA-IN-TANCD         PIC   X(02).
 01  PARA-IN-OUTSYU        PIC   X(01).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-IN-BUMON
                                       PARA-IN-TANCD
                                       PARA-IN-OUTSYU.
 DECLARATIVES.
*
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SMSMNTW3.
     MOVE      "SMSMNTW3"   TO   AB-FILE.
     MOVE      SW3-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SMSMNTW2.
     MOVE      "SMSMNTW2"   TO   AB-FILE.
     MOVE      SW2-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TANMS1.
     MOVE      "TANMS1  "   TO   AB-FILE.
     MOVE      TAN-STATUS   TO   AB-STS.
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
     IF       PARA-IN-OUTSYU  =  "1"
              OPEN     INPUT  SMSMNTW3  TANMS1
              OPEN     OUTPUT PRTF
     ELSE
              OPEN     INPUT  SMSMNTW2  TANMS1
              OPEN     OUTPUT PRTF
     END-IF.
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
*商品名称マスタ　取込ファイルスタート
     IF  PARA-IN-OUTSYU  = "1"
         PERFORM  SMSMNTW3-START-SEC
     ELSE
         PERFORM  SMSMNTW2-START-SEC
     END-IF.
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
*商品名称マスタ　取込ファイル読込
     IF  PARA-IN-OUTSYU  = "1"
         PERFORM SMSMNTW3-READ-SEC
     ELSE
         PERFORM SMSMNTW2-READ-SEC
     END-IF.
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
* 商品名称マスタ　取込ファイル3 スタート
****************************************************************
 SMSMNTW3-START-SEC          SECTION.
*
     MOVE    "SMSMNTW3-START-SEC" TO   S-NAME.
*
     MOVE     SPACE               TO   SW3-REC.
     INITIALIZE                        SW3-REC.
*
     MOVE     " "                 TO   SW3-F11.
     MOVE     SPACE               TO   SW3-F13.
     MOVE     SPACE               TO   SW3-F14.
*
     START  SMSMNTW3  KEY  IS  >=      SW3-F11  SW3-F13
                                       SW3-F14
            INVALID
            MOVE    "END"         TO   END-FLG
     END-START.
*
 SMSMNTW3-START-EXIT.
     EXIT.
*
****************************************************************
*    商品名称マスタ　取込ファイル3   読込
****************************************************************
 SMSMNTW3-READ-SEC           SECTION.
*
     MOVE    "SMSMNTW3-READ-SEC"   TO   S-NAME.
*
     READ     SMSMNTW3  AT  END
              MOVE     "END"      TO   END-FLG
*             DISPLAY "AAA" UPON CONS
              GO                  TO   SMSMNTW3-READ-EXIT
     END-READ.
*件数カウント
     ADD      1                   TO   SMSMNTW3-READ-CNT.
*
*モードブレイク判定
     IF       SW3-F11   NOT =  BK-F11
              MOVE     "BRK"      TO   BRK-MODE
              MOVE      SW3-F11   TO   BK-F11
     ELSE
              MOVE     "   "      TO   BRK-MODE
     END-IF.
*
 SMSMNTW3-READ-EXIT.
     EXIT.
*
****************************************************************
* 商品名称マスタ　取込ファイル2 スタート
****************************************************************
 SMSMNTW2-START-SEC          SECTION.
*
     MOVE    "SMSMNTW2-START-SEC" TO   S-NAME.
*
     MOVE     SPACE               TO   SW2-REC.
     INITIALIZE                        SW2-REC.
*
     MOVE     "1"                 TO   SW2-F060.
     MOVE     " "                 TO   SW2-F11.
     MOVE     SPACE               TO   SW2-F13.
     MOVE     SPACE               TO   SW2-F14.
*
     START  SMSMNTW2  KEY  IS  >=      SW2-F060 SW2-F11
                                       SW2-F13  SW2-F14
            INVALID
            MOVE    "END"         TO   END-FLG
     END-START.
*
 SMSMNTW2-START-EXIT.
     EXIT.
*
****************************************************************
*    商品名称マスタ　取込ファイル2   読込
****************************************************************
 SMSMNTW2-READ-SEC           SECTION.
*
     MOVE    "SMSMNTW2-READ-SEC"   TO   S-NAME.
*
     READ     SMSMNTW2  AT  END
              MOVE     "END"      TO   END-FLG
*             DISPLAY "AAA" UPON CONS
              GO                  TO   SMSMNTW2-READ-EXIT
     END-READ.
*エラー区分チェック
*    DISPLAY "SW2-F060 = "SW2-F060 UPON CONS.
     IF       SW2-F060  NOT =  "1"
              MOVE     "END"      TO   END-FLG
*             DISPLAY "BBB" UPON CONS
              GO                  TO   SMSMNTW2-READ-EXIT
     END-IF.
*件数カウント
     ADD      1                   TO   SMSMNTW2-READ-CNT.
*モードブレイク判定
     IF       SW2-F11   NOT =  BK-F11
              MOVE     "BRK"      TO   BRK-MODE
              MOVE      SW2-F11   TO   BK-F11
     ELSE
              MOVE     "   "      TO   BRK-MODE
     END-IF.
*
 SMSMNTW2-READ-EXIT.
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
     IF  PARA-IN-OUTSYU  = "1"
         PERFORM  SMSMNTW3-READ-SEC
     ELSE
         PERFORM  SMSMNTW2-READ-SEC
     END-IF.
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
*    モード名セット
     IF  PARA-IN-OUTSYU  = "1"
         IF   SW3-F11    = "1"
              MOVE NC"【登録分】"      TO   HD00-MODE
         ELSE
              IF   SW3-F11    = "2"
                   MOVE NC"【更新分】" TO   HD00-MODE
              END-IF
         END-IF
     ELSE
         IF   SW2-F11    = "1"
              MOVE NC"【登録分】"      TO   HD00-MODE
         ELSE
              IF   SW2-F11    = "2"
                   MOVE NC"【更新分】" TO   HD00-MODE
         END-IF
     END-IF.
*    帳票名セット
     IF  PARA-IN-OUTSYU  = "1"
         MOVE NC"取込チェックリスト　＞"    TO   HD00-OUTSYUN
     ELSE
         MOVE NC"取込エラーリスト　＞"      TO   HD00-OUTSYUN
     END-IF.
*    システム日付セット
     MOVE     SYS-DATEW(1:4)      TO   HD00-YYYY.
     MOVE     SYS-DATEW(5:2)      TO   HD00-MM.
     MOVE     SYS-DATEW(7:2)      TO   HD00-DD.
*    システム時刻セット
     MOVE     WK-TIME-HM(1:2)     TO   HD00-HH.
     MOVE     WK-TIME-HM(3:2)     TO   HD00-SS.
     MOVE     WK-TIME-HM(5:2)     TO   HD00-MS.
*    担当者名取得
     MOVE     PARA-IN-BUMON       TO   TAN-F01 HD000-TRBUMON.
     MOVE     PARA-IN-TANCD       TO   TAN-F02 HD000-TRTANTO.
     PERFORM  TANMS1-READ-SEC.
     IF  TANMS1-INV-FLG = SPACE
               MOVE  TAN-F03      TO   HD000-TRTANNM
     ELSE
               MOVE  ALL NC"？"   TO   HD000-TRTANNM
     END-IF.
*    取込日付セット
     IF  PARA-IN-OUTSYU  = "1"
         MOVE SW3-F01(1:4)      TO   HD01-YYYY
         MOVE SW3-F01(5:2)      TO   HD01-MM
         MOVE SW3-F01(7:2)      TO   HD01-DD
     ELSE
         MOVE SW2-F01(1:4)      TO   HD01-YYYY
         MOVE SW2-F01(5:2)      TO   HD01-MM
         MOVE SW2-F01(7:2)      TO   HD01-DD
     END-IF.
*    時刻セット
     IF  PARA-IN-OUTSYU  = "1"
         MOVE SW3-F02(1:2)      TO   HD01-HH
         MOVE SW3-F02(3:2)      TO   HD01-SS
         MOVE SW3-F02(5:2)      TO   HD01-MS
     ELSE
         MOVE SW2-F02(1:2)      TO   HD01-HH
         MOVE SW2-F02(3:2)      TO   HD01-SS
         MOVE SW2-F02(5:2)      TO   HD01-MS
     END-IF.
*    ヘッダ印刷
*
     WRITE    PRT-REC       FROM  KEN01 AFTER  1.
     WRITE    PRT-REC       FROM  KEN02 AFTER  1.
     WRITE    PRT-REC       FROM  KEN02 AFTER  1.
     WRITE    PRT-REC       FROM  KEN02 AFTER  1.
     WRITE    PRT-REC       FROM  KEN01 AFTER  1.
*
     WRITE    PRT-REC       FROM  HD00  AFTER  2.
     WRITE    PRT-REC       FROM  HD000 AFTER  2.
     WRITE    PRT-REC       FROM  HD01  AFTER  1.
     WRITE    PRT-REC       FROM  HD02  AFTER  1.
     WRITE    PRT-REC       FROM  HD03  AFTER  1.
     WRITE    PRT-REC       FROM  HD04  AFTER  1.
     WRITE    PRT-REC       FROM  HD045 AFTER  1.
     WRITE    PRT-REC       FROM  HD05  AFTER  1.
     WRITE    PRT-REC       FROM  HD02  AFTER  1.
     MOVE     SPACE               TO    PRT-REC.
     WRITE    PRT-REC                   AFTER  1.
*行カウントアップ
     MOVE     17                  TO    LINE-CNT.
*
 HEAD-WT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    明細印字
*--------------------------------------------------------------*
 MEISAI-WT-SEC          SECTION.
     MOVE    "MEISAI-WT-SEC"      TO   S-NAME.
*   改頁判定
*    IF     ( LINE-CNT  >   50  ) OR
     IF     ( LINE-CNT  >   48  ) OR
            ( BRK-MODE = "BRK" )
              PERFORM HEAD-WT-SEC
     END-IF.
*   商品コード
     IF  PARA-IN-OUTSYU  = "1"
         MOVE  SW3-F13      TO   MS01-SHOCD
     ELSE
         MOVE  SW2-F13      TO   MS01-SHOCD
     END-IF.
*   品単コード
     IF  PARA-IN-OUTSYU  = "1"
         MOVE  SW3-F14(1:5) TO   MS01-HINTAN(1:5)
         MOVE  "-"          TO   MS01-HINTAN(6:1)
         MOVE  SW3-F14(6:2) TO   MS01-HINTAN(7:2)
         MOVE  "-"          TO   MS01-HINTAN(9:1)
         MOVE  SW3-F14(8:1) TO   MS01-HINTAN(10:1)
     ELSE
         MOVE  SW2-F14(1:5) TO   MS01-HINTAN(1:5)
         MOVE  "-"          TO   MS01-HINTAN(6:1)
         MOVE  SW2-F14(6:2) TO   MS01-HINTAN(7:2)
         MOVE  "-"          TO   MS01-HINTAN(9:1)
         MOVE  SW2-F14(8:1) TO   MS01-HINTAN(10:1)
     END-IF.
*   ＪＡＮコード
     IF  PARA-IN-OUTSYU  = "1"
         IF    SW3-F11   = "1"
               MOVE  SW3-F232         TO   MS01-JANCD
         ELSE
               IF    SW3-F231  = "Y"
                     MOVE  SW3-F232   TO   MS01-JANCD
               ELSE
                     MOVE  SPACE      TO   MS01-JANCD
               END-IF
         END-IF
     ELSE
         IF    SW2-F11   = "1"
               MOVE  SW2-F232         TO   MS01-JANCD
         ELSE
               IF    SW2-F231  = "Y"
                     MOVE  SW2-F232   TO   MS01-JANCD
               ELSE
                     MOVE  SPACE      TO   MS01-JANCD
               END-IF
         END-IF
     END-IF.
*   主仕入先
     IF  PARA-IN-OUTSYU  = "1"
         IF    SW3-F11   = "1"
               MOVE  SW3-F222         TO   MS01-SHICD
         ELSE
               IF    SW3-F221  = "Y"
                     MOVE  SW3-F222   TO   MS01-SHICD
               ELSE
                     MOVE  SPACE      TO   MS01-SHICD
               END-IF
         END-IF
     ELSE
         IF    SW2-F11   = "1"
               MOVE  SW2-F222         TO   MS01-SHICD
         ELSE
               IF    SW2-F221  = "Y"
                     MOVE  SW2-F222   TO   MS01-SHICD
               ELSE
                     MOVE  SPACE      TO   MS01-SHICD
               END-IF
         END-IF
     END-IF.
*   入数
     IF  PARA-IN-OUTSYU  = "1"
         IF    SW3-F11   = "1"
               MOVE  SW3-F242         TO   MS01-IRISU4
         ELSE
               IF    SW3-F241  = "Y"
                     MOVE  SW3-F242   TO   MS01-IRISU4
               ELSE
                     MOVE  SPACE      TO   MS01-IRISU
               END-IF
         END-IF
     ELSE
         IF    SW2-F11   = "1"
               MOVE  SW2-F242         TO   MS01-IRISU4
         ELSE
               IF    SW2-F241  = "Y"
                     MOVE  SW2-F242   TO   MS01-IRISU4
               ELSE
                     MOVE  SPACE      TO   MS01-IRISU
               END-IF
         END-IF
     END-IF.
*   仕入単価
     IF  PARA-IN-OUTSYU  = "1"
         IF    SW3-F11   = "1"
               MOVE  SW3-F192         TO   MS01-STANKA9
         ELSE
               IF    SW3-F191  = "Y"
                     MOVE  SW3-F192   TO   MS01-STANKA9
               ELSE
                     MOVE  SPACE      TO   MS01-STANKA
               END-IF
         END-IF
     ELSE
         IF    SW2-F11   = "1"
               MOVE  SW2-F192         TO   MS01-STANKA9
         ELSE
               IF    SW2-F191  = "Y"
                     MOVE  SW2-F192   TO   MS01-STANKA9
               ELSE
                     MOVE  SPACE      TO   MS01-STANKA
               END-IF
         END-IF
     END-IF.
*   基準原価
     IF  PARA-IN-OUTSYU  = "1"
         IF    SW3-F11   = "1"
               MOVE  SW3-F202         TO   MS01-KGENKA9
         ELSE
               IF    SW3-F201  = "Y"
                     MOVE  SW3-F202   TO   MS01-KGENKA9
               ELSE
                     MOVE  SPACE      TO   MS01-KGENKA
               END-IF
         END-IF
     ELSE
         IF    SW2-F11   = "1"
               MOVE  SW2-F202         TO   MS01-KGENKA9
         ELSE
               IF    SW2-F201  = "Y"
                     MOVE  SW2-F202   TO   MS01-KGENKA9
               ELSE
                     MOVE  SPACE      TO   MS01-KGENKA
               END-IF
         END-IF
     END-IF.
*   基準売価
     IF  PARA-IN-OUTSYU  = "1"
         IF    SW3-F11   = "1"
               MOVE  SW3-F212         TO   MS01-KBAIKA9
         ELSE
               IF    SW3-F211  = "Y"
                     MOVE  SW3-F212   TO   MS01-KBAIKA9
               ELSE
                     MOVE  SPACE      TO   MS01-KBAIKA
               END-IF
         END-IF
     ELSE
         IF    SW2-F11   = "1"
               MOVE  SW2-F212         TO   MS01-KBAIKA9
         ELSE
               IF    SW2-F211  = "Y"
                     MOVE  SW2-F212   TO   MS01-KBAIKA9
               ELSE
                     MOVE  SPACE      TO   MS01-KBAIKA
               END-IF
         END-IF
     END-IF.
*   D365商品CD
     IF  PARA-IN-OUTSYU  = "1"
         IF    SW3-F11   = "1"
               MOVE  SW3-F332         TO   MS02-D365SHOCD
         ELSE
               IF    SW3-F331  = "Y"
                     MOVE  SW3-F332   TO   MS02-D365SHOCD
               ELSE
                     MOVE  SPACE      TO   MS02-D365SHOCD
               END-IF
         END-IF
     ELSE
         IF    SW2-F11   = "1"
               MOVE  SW2-F332         TO   MS02-D365SHOCD
         ELSE
               IF    SW2-F331  = "Y"
                     MOVE  SW2-F332   TO   MS02-D365SHOCD
               ELSE
                     MOVE  SPACE      TO   MS02-D365SHOCD
               END-IF
         END-IF
     END-IF.
*   D365JANCD
     IF  PARA-IN-OUTSYU  = "1"
         IF    SW3-F11   = "1"
               MOVE  SW3-F342         TO   MS02-D365JANCD
         ELSE
               IF    SW3-F341  = "Y"
                     MOVE  SW3-F342   TO   MS02-D365JANCD
               ELSE
                     MOVE  SPACE      TO   MS02-D365JANCD
               END-IF
         END-IF
     ELSE
         IF    SW2-F11   = "1"
               MOVE  SW2-F342         TO   MS02-D365JANCD
         ELSE
               IF    SW2-F341  = "Y"
                     MOVE  SW2-F342   TO   MS02-D365JANCD
               ELSE
                     MOVE  SPACE      TO   MS02-D365JANCD
               END-IF
         END-IF
     END-IF.
*   区分－定番区分
     IF  PARA-IN-OUTSYU  = "1"
         IF    SW3-F11   = "1"
               MOVE  SW3-F322         TO   MS02-KBN-TEIBAN
         ELSE
               IF    SW3-F321  = "Y"
                     MOVE  SW3-F322   TO   MS02-KBN-TEIBAN
               ELSE
                     MOVE  SPACE      TO   MS02-KBN-TEIBAN
               END-IF
         END-IF
     ELSE
         IF    SW2-F11   = "1"
               MOVE  SW2-F322         TO   MS02-KBN-TEIBAN
         ELSE
               IF    SW2-F321  = "Y"
                     MOVE  SW2-F322   TO   MS02-KBN-TEIBAN
               ELSE
                     MOVE  SPACE      TO   MS02-KBN-TEIBAN
               END-IF
         END-IF
     END-IF.
*   区分－商品分類区分
     IF  PARA-IN-OUTSYU  = "1"
         IF    SW3-F11   = "1"
               MOVE  SW3-F302         TO   MS02-KBN-BUNRUI
         ELSE
               IF    SW3-F301  = "Y"
                     MOVE  SW3-F302   TO   MS02-KBN-BUNRUI
               ELSE
                     MOVE  SPACE      TO   MS02-KBN-BUNRUI
               END-IF
         END-IF
     ELSE
         IF    SW2-F11   = "1"
               MOVE  SW2-F302         TO   MS02-KBN-BUNRUI
         ELSE
               IF    SW2-F301  = "Y"
                     MOVE  SW2-F302   TO   MS02-KBN-BUNRUI
               ELSE
                     MOVE  SPACE      TO   MS02-KBN-BUNRUI
               END-IF
         END-IF
     END-IF.
*   区分－サカタ２０分類
     IF  PARA-IN-OUTSYU  = "1"
         IF    SW3-F11   = "1"
               MOVE  SW3-F262         TO   MS02-KBN-20BUNR
         ELSE
               IF    SW3-F261  = "Y"
                     MOVE  SW3-F262   TO   MS02-KBN-20BUNR
               ELSE
                     MOVE  SPACE      TO   MS02-KBN-20BUNR
               END-IF
         END-IF
     ELSE
         IF    SW2-F11   = "1"
               MOVE  SW2-F262         TO   MS02-KBN-20BUNR
         ELSE
               IF    SW2-F261  = "Y"
                     MOVE  SW2-F262   TO   MS02-KBN-20BUNR
               ELSE
                     MOVE  SPACE      TO   MS02-KBN-20BUNR
               END-IF
         END-IF
     END-IF.
*   区分－物流束区分
     IF  PARA-IN-OUTSYU  = "1"
         IF    SW3-F11   = "1"
               MOVE  SW3-F292         TO   MS02-KBN-TABA
         ELSE
               IF    SW3-F291  = "Y"
                     MOVE  SW3-F292   TO   MS02-KBN-TABA
               ELSE
                     MOVE  SPACE      TO   MS02-KBN-TABA
               END-IF
         END-IF
     ELSE
         IF    SW2-F11   = "1"
               MOVE  SW2-F292         TO   MS02-KBN-TABA
         ELSE
               IF    SW2-F291  = "Y"
                     MOVE  SW2-F292   TO   MS02-KBN-TABA
               ELSE
                     MOVE  SPACE      TO   MS02-KBN-TABA
               END-IF
         END-IF
     END-IF.
*   区分－廃盤区分
     IF  PARA-IN-OUTSYU  = "1"
         IF    SW3-F11   = "1"
               MOVE  SW3-F252         TO   MS02-KBN-HAIBAN
         ELSE
               IF    SW3-F251  = "Y"
                     MOVE  SW3-F252   TO   MS02-KBN-HAIBAN
               ELSE
                     MOVE  SPACE      TO   MS02-KBN-HAIBAN
               END-IF
         END-IF
     ELSE
         IF    SW2-F11   = "1"
               MOVE  SW2-F252         TO   MS02-KBN-HAIBAN
         ELSE
               IF    SW2-F251  = "Y"
                     MOVE  SW2-F252   TO   MS02-KBN-HAIBAN
               ELSE
                     MOVE  SPACE      TO   MS02-KBN-HAIBAN
               END-IF
         END-IF
     END-IF.
*   区分－管理区分
     IF  PARA-IN-OUTSYU  = "1"
         IF    SW3-F11   = "1"
               MOVE  SW3-F312         TO   MS02-KBN-KANRI
         ELSE
               IF    SW3-F311  = "Y"
                     MOVE  SW3-F312   TO   MS02-KBN-KANRI
               ELSE
                     MOVE  SPACE      TO   MS02-KBN-KANRI
               END-IF
         END-IF
     ELSE
         IF    SW2-F11   = "1"
               MOVE  SW2-F312         TO   MS02-KBN-KANRI
         ELSE
               IF    SW2-F311  = "Y"
                     MOVE  SW2-F312   TO   MS02-KBN-KANRI
               ELSE
                     MOVE  SPACE      TO   MS02-KBN-KANRI
               END-IF
         END-IF
     END-IF.
*   区分－小売区分
     IF  PARA-IN-OUTSYU  = "1"
         IF    SW3-F11   = "1"
               MOVE  SW3-F272         TO   MS02-KBN-KOURI
         ELSE
               IF    SW3-F271  = "Y"
                     MOVE  SW3-F272   TO   MS02-KBN-KOURI
               ELSE
                     MOVE  SPACE      TO   MS02-KBN-KOURI
               END-IF
         END-IF
     ELSE
         IF    SW2-F11   = "1"
               MOVE  SW2-F272         TO   MS02-KBN-KOURI
         ELSE
               IF    SW2-F271  = "Y"
                     MOVE  SW2-F272   TO   MS02-KBN-KOURI
               ELSE
                     MOVE  SPACE      TO   MS02-KBN-KOURI
               END-IF
         END-IF
     END-IF.
*   区分－振替区分
     IF  PARA-IN-OUTSYU  = "1"
         IF    SW3-F11   = "1"
               MOVE  SW3-F282         TO   MS02-KBN-FURI
         ELSE
               IF    SW3-F281  = "Y"
                     MOVE  SW3-F282   TO   MS02-KBN-FURI
               ELSE
                     MOVE  SPACE      TO   MS02-KBN-FURI
               END-IF
         END-IF
     ELSE
         IF    SW2-F11   = "1"
               MOVE  SW2-F282         TO   MS02-KBN-FURI
         ELSE
               IF    SW2-F281  = "Y"
                     MOVE  SW2-F282   TO   MS02-KBN-FURI
               ELSE
                     MOVE  SPACE      TO   MS02-KBN-FURI
               END-IF
         END-IF
     END-IF.
*   商品名1
     IF  PARA-IN-OUTSYU  = "1"
         IF    SW3-F11   = "1"
               MOVE  SW3-F152         TO   MS03-SHONM1
         ELSE
               IF    SW3-F151  = "Y"
                     MOVE  SW3-F152   TO   MS03-SHONM1
               ELSE
                     MOVE  SPACE      TO   MS03-SHONM1
               END-IF
         END-IF
     ELSE
         IF    SW2-F11   = "1"
               MOVE  SW2-F152         TO   MS03-SHONM1
         ELSE
               IF    SW2-F151  = "Y"
                     MOVE  SW2-F152   TO   MS03-SHONM1
               ELSE
                     MOVE  SPACE      TO   MS03-SHONM1
               END-IF
         END-IF
     END-IF.
*   商品名2
     IF  PARA-IN-OUTSYU  = "1"
         IF    SW3-F11   = "1"
               MOVE  SW3-F162         TO   MS03-SHONM2
         ELSE
               IF    SW3-F161  = "Y"
                     MOVE  SW3-F162   TO   MS03-SHONM2
               ELSE
                     MOVE  SPACE      TO   MS03-SHONM2
               END-IF
         END-IF
     ELSE
         IF    SW2-F11   = "1"
               MOVE  SW2-F162         TO   MS03-SHONM2
         ELSE
               IF    SW2-F161  = "Y"
                     MOVE  SW2-F162   TO   MS03-SHONM2
               ELSE
                     MOVE  SPACE      TO   MS03-SHONM2
               END-IF
         END-IF
     END-IF.
*   商品カナ名1
     IF  PARA-IN-OUTSYU  = "1"
         IF    SW3-F11   = "1"
               MOVE  SW3-F172         TO   MS03-SHOKANA1
         ELSE
               IF    SW3-F171  = "Y"
                     MOVE  SW3-F172   TO   MS03-SHOKANA1
               ELSE
                     MOVE  SPACE      TO   MS03-SHOKANA1
               END-IF
         END-IF
     ELSE
         IF    SW2-F11   = "1"
               MOVE  SW2-F172         TO   MS03-SHOKANA1
         ELSE
               IF    SW2-F171  = "Y"
                     MOVE  SW2-F172   TO   MS03-SHOKANA1
               ELSE
                     MOVE  SPACE      TO   MS03-SHOKANA1
               END-IF
         END-IF
     END-IF.
*   商品カナ名2
     IF  PARA-IN-OUTSYU  = "1"
         IF    SW3-F11   = "1"
               MOVE  SW3-F182         TO   MS03-SHOKANA2
         ELSE
               IF    SW3-F181  = "Y"
                     MOVE  SW3-F182   TO   MS03-SHOKANA2
               ELSE
                     MOVE  SPACE      TO   MS03-SHOKANA2
               END-IF
         END-IF
     ELSE
         IF    SW2-F11   = "1"
               MOVE  SW2-F182         TO   MS03-SHOKANA2
         ELSE
               IF    SW2-F181  = "Y"
                     MOVE  SW2-F182   TO   MS03-SHOKANA2
               ELSE
                     MOVE  SPACE      TO   MS03-SHOKANA2
               END-IF
         END-IF
     END-IF.
*2020.05.28↓
*   オーダー区分
     IF  PARA-IN-OUTSYU  = "1"
         IF    SW3-F11   = "1"
               MOVE  SW3-F352         TO   MS03-ORDER
         ELSE
               IF    SW3-F351  = "Y"
                     MOVE  SW3-F352   TO   MS03-ORDER
               ELSE
                     MOVE  SPACE      TO   MS03-ORDER
               END-IF
         END-IF
     ELSE
         IF    SW2-F11   = "1"
               MOVE  SW2-F352         TO   MS03-ORDER
         ELSE
               IF    SW2-F351  = "Y"
                     MOVE  SW2-F352   TO   MS03-ORDER
               ELSE
                     MOVE  SPACE      TO   MS03-ORDER
               END-IF
         END-IF
     END-IF.
*2020.05.28↑
*
*   エラー区分１
     IF  PARA-IN-OUTSYU  = "1"
         IF    SW3-F061 = " "
               MOVE SPACE        TO   MS01-ERR1
         ELSE
               MOVE NC"×"       TO   MS01-ERR1
         END-IF
     ELSE
         IF    SW2-F061 = " "
               MOVE SPACE        TO   MS01-ERR1
         ELSE
               MOVE NC"×"       TO   MS01-ERR1
         END-IF
     END-IF.
*    エラー区分２
     IF  PARA-IN-OUTSYU  = "1"
         IF    SW3-F062 = " "
               MOVE SPACE        TO   MS01-ERR2
         ELSE
               MOVE NC"×"       TO   MS01-ERR2
         END-IF
     ELSE
         IF    SW2-F062 = " "
               MOVE SPACE        TO   MS01-ERR2
         ELSE
               MOVE NC"×"       TO   MS01-ERR2
         END-IF
     END-IF.
*    エラー区分３
     IF  PARA-IN-OUTSYU  = "1"
         IF    SW3-F063 = " "
               MOVE SPACE        TO   MS01-ERR3
         ELSE
               MOVE NC"×"       TO   MS01-ERR3
         END-IF
     ELSE
         IF    SW2-F063 = " "
               MOVE SPACE        TO   MS01-ERR3
         ELSE
               MOVE NC"×"       TO   MS01-ERR3
         END-IF
     END-IF.
*    エラー区分４
     IF  PARA-IN-OUTSYU  = "1"
         IF    SW3-F064 = " "
               MOVE SPACE        TO   MS01-ERR4
         ELSE
               MOVE NC"×"       TO   MS01-ERR4
         END-IF
     ELSE
         IF    SW2-F064 = " "
               MOVE SPACE        TO   MS01-ERR4
         ELSE
               MOVE NC"×"       TO   MS01-ERR4
         END-IF
     END-IF.
*    エラー区分５
     IF  PARA-IN-OUTSYU  = "1"
         IF    SW3-F065 = " "
               MOVE SPACE        TO   MS01-ERR5
         ELSE
               MOVE NC"×"       TO   MS01-ERR5
         END-IF
     ELSE
         IF    SW2-F065 = " "
               MOVE SPACE        TO   MS01-ERR5
         ELSE
               MOVE NC"×"       TO   MS01-ERR5
         END-IF
     END-IF.
*    エラー区分６
     IF  PARA-IN-OUTSYU  = "1"
         IF    SW3-F066 = " "
               MOVE SPACE        TO   MS01-ERR6
         ELSE
               MOVE NC"×"       TO   MS01-ERR6
         END-IF
     ELSE
         IF    SW2-F066 = " "
               MOVE SPACE        TO   MS01-ERR6
         ELSE
               MOVE NC"×"       TO   MS01-ERR6
         END-IF
     END-IF.
*    エラー区分７
     IF  PARA-IN-OUTSYU  = "1"
         IF    SW3-F067 = " "
               MOVE SPACE        TO   MS01-ERR7
         ELSE
               MOVE NC"×"       TO   MS01-ERR7
         END-IF
     ELSE
         IF    SW2-F067 = " "
               MOVE SPACE        TO   MS01-ERR7
         ELSE
               MOVE NC"×"       TO   MS01-ERR7
         END-IF
     END-IF.
*    エラー区分８
     IF  PARA-IN-OUTSYU  = "1"
         IF    SW3-F068 = " "
               MOVE SPACE        TO   MS01-ERR8
         ELSE
               MOVE NC"×"       TO   MS01-ERR8
         END-IF
     ELSE
         IF    SW2-F068 = " "
               MOVE SPACE        TO   MS01-ERR8
         ELSE
               MOVE NC"×"       TO   MS01-ERR8
         END-IF
     END-IF.
*    エラー区分９
     IF  PARA-IN-OUTSYU  = "1"
         IF    SW3-F069 = " "
               MOVE SPACE        TO   MS01-ERR9
         ELSE
               MOVE NC"×"       TO   MS01-ERR9
         END-IF
     ELSE
         IF    SW2-F069 = " "
               MOVE SPACE        TO   MS01-ERR9
         ELSE
               MOVE NC"×"       TO   MS01-ERR9
         END-IF
     END-IF.
*    エラー区分１０("1"＝エラー無し)
     IF  PARA-IN-OUTSYU  = "1"
         IF    SW3-F06A = " "
               MOVE SPACE        TO   MS01-ERR10
         ELSE
               MOVE NC"○"       TO   MS01-ERR10
         END-IF
     ELSE
         IF    SW2-F06A = " "
               MOVE SPACE        TO   MS01-ERR10
         ELSE
               MOVE NC"○"       TO   MS01-ERR10
         END-IF
     END-IF.
*    明細印刷
     WRITE    PRT-REC       FROM  MS01  AFTER  1.
     WRITE    PRT-REC       FROM  MS02  AFTER  1.
     WRITE    PRT-REC       FROM  MS03  AFTER  1.
     WRITE    PRT-REC       FROM  HD021 AFTER  1.
*行カウント
     ADD      4                   TO    LINE-CNT.
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
*商品名称マスタ　
     IF  PARA-IN-OUTSYU  = "1"
         DISPLAY "SMSMNTW3 PRINT-CNT = " SMSMNTW3-READ-CNT
                                                       UPON CONS
     ELSE
         DISPLAY "SMSMNTW2 PRINT-CNT = " SMSMNTW2-READ-CNT
                                                       UPON CONS
     END-IF.
*
     IF       PARA-IN-OUTSYU  =  "1"
              CLOSE  SMSMNTW3  TANMS1  PRTF
     ELSE
              CLOSE  SMSMNTW2  TANMS1  PRTF
     END-IF.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
****************************************************************
*    担当者マスタ索引
****************************************************************
 TANMS1-READ-SEC           SECTION.
     MOVE "TANMS1-READ-SEC"       TO   S-NAME.
*
     READ       TANMS1
         INVALID       MOVE "INV" TO   TANMS1-INV-FLG
         NOT  INVALID  MOVE SPACE TO   TANMS1-INV-FLG
     END-READ.
*
 TANMS1-READ-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
