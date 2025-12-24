# SJS0083L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SJS0083L.COB`

## ソースコード

```cobol
****************************************************************
*
*    ユーザ　　　　名：　サカタのタネ　　　殿
*    システム　　　名：　実績管理システム
*    プログラム　　名：　週別仕入先別仕入実績表
*    作成者　　　　　：　飯田/NAV　
*    作成日　　　　　：　2011.11.24
*    更新履歴        ：
*
****************************************************************
 IDENTIFICATION      DIVISION.
 PROGRAM-ID.         SJS0083L.
 AUTHOR.             NAV.
 DATE-WRITTEN.       2011.11.21.
*
 ENVIRONMENT         DIVISION.
 CONFIGURATION       SECTION.
 SPECIAL-NAMES.
         YA        IS   YA
         YA-21     IS   YA-21
         YB        IS   YB
     CONSOLE       IS   CONS.
*
 INPUT-OUTPUT        SECTION.
 FILE-CONTROL.
*画面Ｆ
     SELECT   DSPF           ASSIGN               TO  GS-DSPF
                             ORGANIZATION         IS  SEQUENTIAL
                             ACCESS MODE          IS  SEQUENTIAL
                             SYMBOLIC DESTINATION IS "DSP"
                             PROCESSING MODE      IS  DSP-PRO
                             GROUP                IS  DSP-GRP
                             FORMAT               IS  DSP-FMT
                             SELECTED FUNCTION    IS  DSP-FNC
                             FILE STATUS          IS  DSP-STA.

*実績週集計ファイル
     SELECT   JISSSYUF       ASSIGN        TO  01-VI-JISSSYU2
                             ORGANIZATION  IS  INDEXED
                             ACCESS MODE   IS  DYNAMIC
                             RECORD KEY    IS
                               JSS-F01  *> 売上仕入区分
                               JSS-F021 *> 年
                               JSS-F022 *> 週番号
                               JSS-F03  *> 取引先
                               JSS-F051 *> 商品ＣＤ
                               JSS-F052 *> 品単ＣＤ
                               JSS-F12  *> 相手商品コード
                             FILE STATUS   IS  JSS-STA.
*実績週集計ファイル（前年）
     SELECT   JISSSB1       ASSIGN        TO  01-VI-JISSSB12
                             ORGANIZATION  IS  INDEXED
                             ACCESS MODE   IS  DYNAMIC
                             RECORD KEY    IS
                               JB1-F01  *> 売上仕入区分
                               JB1-F021 *> 年
                               JB1-F022 *> 週番号
                               JB1-F03  *> 取引先
                               JB1-F051 *> 商品ＣＤ
                               JB1-F052 *> 品単ＣＤ
                               JB1-F12  *> 相手商品コード
                             FILE STATUS   IS  JB1-STA.

*実績週集計ファイル（２年前）
     SELECT   JISSSB2       ASSIGN        TO  01-VI-JISSSB22
                             ORGANIZATION  IS  INDEXED
                             ACCESS MODE   IS  DYNAMIC
                             RECORD KEY    IS
                               JB2-F01  *> 売上仕入区分
                               JB2-F021 *> 年
                               JB2-F022 *> 週番号
                               JB2-F03  *> 取引先
                               JB2-F051 *> 商品ＣＤ
                               JB2-F052 *> 品単ＣＤ
                               JB2-F12  *> 相手商品コード
                             FILE STATUS   IS  JB2-STA.

*実績週集計ファイル（３年前）
     SELECT   JISSSB3       ASSIGN        TO  01-VI-JISSSB32
                             ORGANIZATION  IS  INDEXED
                             ACCESS MODE   IS  DYNAMIC
                             RECORD KEY    IS
                               JB3-F01  *> 売上仕入区分
                               JB3-F021 *> 年
                               JB3-F022 *> 週番号
                               JB3-F03  *> 取引先
                               JB3-F051 *> 商品ＣＤ
                               JB3-F052 *> 品単ＣＤ
                               JB3-F12  *> 相手商品コード
                             FILE STATUS   IS  JB3-STA.
*部門取引先マスタ
     SELECT   BUTOKMF        ASSIGN        TO  01-VI-BUTOKML1
                             ORGANIZATION  IS  INDEXED
                             ACCESS  MODE  IS  RANDOM
                             RECORD  KEY   IS  BUT-F01
                             FILE STATUS   IS  BUT-STA.
*条件ファイル
     SELECT   HJYOKEN        ASSIGN        TO  01-VI-JYOKEN1
                             ORGANIZATION  IS  INDEXED
                             ACCESS MODE   IS  SEQUENTIAL
                             RECORD KEY    IS  JYO-F01
                                               JYO-F02
                             FILE STATUS   IS  JYO-STA.
*日次更新条件ファイル
     SELECT   HIDUKEF        ASSIGN        TO  01-VI-HIDUKEL1
                             ORGANIZATION  IS  INDEXED
                             ACCESS MODE   IS  SEQUENTIAL
                             RECORD KEY    IS
                               JHM-F01 *> 日時更新日付
                             FILE STATUS   IS  JHM-STA.

*プリントファイル
     SELECT   PRINTF         ASSIGN        TO  LP-04.
*
**************************************************************
 DATA                DIVISION.
**************************************************************
*=============================================================
 FILE                SECTION.
*=============================================================
*画面Ｆ
 FD  DSPF.
     COPY     FJS00831  OF  XMDLIB
     JOINING  DSP       AS  PREFIX.
*実績週集計ファイル
 FD  JISSSYUF.
     COPY     JISSSYUF  OF  XFDLIB
     JOINING  JSS       AS  PREFIX.
*実績週集計ファイル（前年)
 FD  JISSSB1.
     COPY     JISSSB1   OF  XFDLIB
     JOINING  JB1       AS  PREFIX.
*実績週集計ファイル（２年前)
 FD  JISSSB2.
     COPY     JISSSB2   OF  XFDLIB
     JOINING  JB2       AS  PREFIX.
*実績週集計ファイル（３年前)
 FD  JISSSB3.
     COPY     JISSSB3   OF  XFDLIB
     JOINING  JB3       AS  PREFIX.
*部門取引先Ｍ
 FD  BUTOKMF.
     COPY     BUTOKMF  OF  XFDLIB
     JOINING  BUT      AS  PREFIX.
*条件ファイル
 FD  HJYOKEN.
     COPY     HJYOKEN  OF  XFDLIB
     JOINING  JYO      AS  PREFIX.
*日次更新条件ファイル
 FD  HIDUKEF.
     COPY     HIDUKEF  OF  XFDLIB
     JOINING  JHM      AS  PREFIX.
*プリントファイル
 FD    PRINTF    LINAGE  IS  66.
 01    P-REC                 PIC  X(200).
*
*=============================================================
 WORKING-STORAGE     SECTION.
*=============================================================
*画面制御用
 01  DSP-CONTROL.
     03  DSP-PRO             PIC  X(02).
     03  DSP-GRP             PIC  X(08).
     03  DSP-FMT             PIC  X(08).
     03  DSP-FNC             PIC  X(04).
*ステータス
 01  STA-AREA.
     03  DSP-STA             PIC  X(02).
     03  JSS-STA             PIC  X(02).
     03  JB1-STA             PIC  X(02).
     03  JB2-STA             PIC  X(02).
     03  JB3-STA             PIC  X(02).
     03  BUT-STA             PIC  X(02).
     03  JYO-STA             PIC  X(02).
     03  JHM-STA             PIC  X(02).
*日付／時刻
 01  TIME-AREA.
     03  WK-TIME                  PIC  9(08)  VALUE  ZERO.
 01  DATE-AREA.
     03  WK-YS                    PIC  9(02)  VALUE  ZERO.
     03  WK-DATE.
         05  WK-Y                 PIC  9(02)  VALUE  ZERO.
         05  WK-M                 PIC  9(02)  VALUE  ZERO.
         05  WK-D                 PIC  9(02)  VALUE  ZERO.
 01  DATE-AREAR1       REDEFINES      DATE-AREA.
     03  SYS-YM                   PIC  9(06).
     03  FILLER                   PIC  9(02).
 01  DATE-AREAR2       REDEFINES      DATE-AREA.
     03  SYS-DATE                 PIC  9(08).
*画面表示日付編集
 01  HEN-DATE.
     03  HEN-DATE-YYYY            PIC  9(04)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-DD              PIC  9(02)  VALUE  ZERO.
*画面表示時刻編集
 01  HEN-TIME.
     03  HEN-TIME-HH              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-SS              PIC  9(02)  VALUE  ZERO.
*メッセージテーブル
 01  MSG-TBL.
     03  MSG-NO01           PIC  N(20)  VALUE
         NC"誤ったＰＦキーが押されました".
     03  MSG-NO02           PIC  N(20)  VALUE
         NC"開始が終了を越えています".
     03  MSG-NO03           PIC  N(20)  VALUE
         NC"対象データはありません".
     03  MSG-NO04           PIC  N(20)  VALUE
         NC"対象データ抽出中です".
     03  MSG-NO05           PIC  N(20)  VALUE
         NC"出力年月日に誤りが有ります".
     03  MSG-NO06           PIC  N(20)  VALUE
         NC"サカタ２０分類条件に誤りが有ります".
     03  MSG-NO07           PIC  N(20)  VALUE
         NC"週番号が取得できません".
     03  MSG-NO08           PIC  N(20)  VALUE
         NC"年を跨いでの出力はできません".
     03  MSG-NO09           PIC  N(20)  VALUE
         NC"　".
     03  MSG-NO10           PIC  N(20)  VALUE
         NC"　".
 01  TBL-MSG-R  REDEFINES MSG-TBL.
     03  TBL-MSG            PIC  N(20)  OCCURS 10 TIMES.
*ＰＦキー
 01  PFK-TBL.
     03  PFK-NO01            PIC  N(30)  VALUE
            NC"_取消　_終了".
     03  PFK-NO01            PIC  N(30)  VALUE
            NC"_取消　_終了　_項目戻り".
 01  TBL-PFK-R       REDEFINES    PFK-TBL.
     03  TBL-PFK             PIC  N(30)  OCCURS  2   TIMES.
*
 01  FLG-AREA.
     03  MAIN-FLG            PIC  9(02)  VALUE  ZERO.
     03  ERR-FLG             PIC  9(02)  VALUE  ZERO.
     03  PFK-FLG             PIC  9(01)  VALUE  ZERO.
     03  JSS-FLG             PIC  9(01)  VALUE  ZERO.
     03  FG-HJYOKEN-END      PIC  X(03).
     03  FG-JISSSYUF-END     PIC  9(01).
     03  FG-JISSSB1-END      PIC  9(01).
     03  FG-JISSSB2-END      PIC  9(01).
     03  FG-JISSSB3-END      PIC  9(01).
     03  FG-JISSEKI-RD-END   PIC  X(03).
     03  FG-JISSSYUF-RVS-END     PIC  9(01).
     03  FG-JISSSB1-RVS-END      PIC  9(01).
     03  FG-JISSSB2-RVS-END      PIC  9(01).
     03  FG-JISSSB3-RVS-END      PIC  9(01).
     03  FG-JISSEKI-RD-RVS-END   PIC  X(03).
     03  FG-ZEN-JISSEKI-RD-END  PIC  X(03).
     03  FG-BUTOKMF-INV      PIC  9(01).
     03  FG-HIDUKEF-RVS-END  PIC  X(03).
     03  FG-STFILE           PIC  9(02).
     03  FG-EDFILE           PIC  9(02).
     03  FG-TOUZEN           PIC  X(03).
     03  FG-RD-TOUZEN        PIC  X(03).
*
 01  CNT-AREA.
     03  PAGE-CNT            PIC  9(07)  VALUE  ZERO.
     03  LINE-CNT            PIC  9(03)  VALUE  ZERO.
     03  WK-LINE-CNT         PIC  9(03)  VALUE  ZERO.
     03  MAX-LINE            PIC  9(02)  VALUE  66.
     03  IN-CNT              PIC  9(07)  VALUE  ZERO.
     03  IN2-CNT             PIC  9(07)  VALUE  ZERO.
     03  IN3-CNT             PIC  9(07)  VALUE  ZERO.
     03  IN4-CNT             PIC  9(07)  VALUE  ZERO.
     03  OT-CNT              PIC  9(07)  VALUE  ZERO.
     03  OT2-CNT             PIC  9(07)  VALUE  ZERO.
     03  OT3-CNT             PIC  9(07)  VALUE  ZERO.
     03  OT4-CNT             PIC  9(07)  VALUE  ZERO.
     03  IN-RVS-CNT          PIC  9(07)  VALUE  ZERO.
     03  IN2-RVS-CNT         PIC  9(07)  VALUE  ZERO.
     03  IN3-RVS-CNT         PIC  9(07)  VALUE  ZERO.
     03  IN4-RVS-CNT         PIC  9(07)  VALUE  ZERO.
     03  OT-RVS-CNT          PIC  9(07)  VALUE  ZERO.
     03  OT2-RVS-CNT         PIC  9(07)  VALUE  ZERO.
     03  OT3-RVS-CNT         PIC  9(07)  VALUE  ZERO.
     03  OT4-RVS-CNT         PIC  9(07)  VALUE  ZERO.

*キー領域
 01  INF-KEY-AREA.
     03  INF-KEY.
         05  INF-F02.
           07  INF-F021      PIC  9(04). *> 年
           07  INF-F022      PIC  9(02). *> 週番号
         05  INF-F03         PIC  X(08). *> 取引先
     03  INF-KEY-R  REDEFINES INF-KEY.
         05  FILLER          PIC  9(04). *> 年
         05  INF-KEY2.
           07  INF-F022-R    PIC  9(02). *> 週番号
           07  INF-F03-R     PIC  X(08). *> 取引先

     03  ZEN-INF-KEY.
         05  ZEN-INF-F02.
           07  ZEN-INF-F021  PIC  9(04). *> 年
           07  ZEN-INF-F022  PIC  9(02). *> 週番号
         05  ZEN-INF-F03     PIC  X(08). *> 取引先
     03  ZEN-INF-KEY-R  REDEFINES ZEN-INF-KEY.
         05  FILLER            PIC  9(04). *> 年
         05  ZEN-INF-KEY2.
           07  ZEN-INF-F022-R  PIC  9(02). *> 週番号
           07  ZEN-INF-F03-R   PIC  X(08). *> 取引先

     03  BRK-KEY.
         05  BRK-F02.
           07  BRK-F021      PIC  9(04). *> 年
           07  BRK-F022      PIC  9(02). *> 週番号
         05  BRK-F03         PIC  X(08). *> 取引先
     03  BRK-KEY-R  REDEFINES BRK-KEY.
         05  FILLER          PIC  9(04). *> 年
         05  BRK-KEY2.
           07  BRK-F022-R    PIC  9(02). *> 週番号
           07  BRK-F03-R     PIC  X(08). *> 取引先

 01  IX                      PIC  9(04).
 01  IX-ICHI-BNR20           PIC  9(03).
 01  IX-BNR20                PIC  9(04).
 01  IX-FIL                  PIC  9(04).
 01  IX-MSGYO-MAX            PIC  9(03).
 01  WK-SHO                  PIC  9(04).
 01  WK-AMARI                PIC  9(04).
 01  WK-STSYUNO              PIC  9(02)  VALUE ZERO.
 01  WK-EDSYUNO              PIC  9(02)  VALUE ZERO.

 01  WK-STFILE               PIC  X(08).
 01  WK-EDFILE               PIC  X(08).
 01  WK-STFILE-TOU           PIC  X(08).
 01  WK-EDFILE-TOU           PIC  X(08).
 01  WK-STFILE-ZEN           PIC  X(08).
 01  WK-EDFILE-ZEN           PIC  X(08).

 01  WK-START-NEN            PIC  9(04).
 01  WK-START-SYUNO          PIC  9(02).
 01  WK-START-TOR            PIC  X(08).
 01  WK-END-NEN              PIC  9(04).
 01  WK-END-SYUNO            PIC  9(02).
 01  WK-END-TOR              PIC  X(08).

 01  WK-RVS-START-NEN        PIC  9(04).
 01  WK-RVS-START-SYUNO      PIC  9(02).
 01  WK-RVS-START-TOR        PIC  X(08).
 01  WK-RVS-END-NEN          PIC  9(04).
 01  WK-RVS-END-SYUNO        PIC  9(02).
 01  WK-RVS-END-TOR          PIC  X(08).
 01  WK-START-NO             PIC  9(04).
 01  WK-ZEN-START-NEN        PIC  9(04).
 01  WK-ZEN-START-SYUNO      PIC  9(02).
 01  WK-ZEN-START-TOR        PIC  X(08).
 01  WK-ZEN-END-NEN          PIC  9(04).
 01  WK-ZEN-END-SYUNO        PIC  9(02).
 01  WK-ZEN-END-TOR          PIC  X(08).

 01  WK-BNR20                PIC  X(02).

*実績ファイル退避
     COPY  JISSSYUF OF XFDLIB  JOINING JSSW  AS PREFIX.
     COPY  JISSSB1  OF XFDLIB  JOINING JB1W  AS PREFIX.
     COPY  JISSSB2  OF XFDLIB  JOINING JB2W  AS PREFIX.
     COPY  JISSSB3  OF XFDLIB  JOINING JB3W  AS PREFIX.

* サカタ２０分類
 01  TB-BNR20-AREA.
     05  TB-BNR20-G  OCCURS 20.
       07  TB-BNR20         PIC  X(02).
       07  TB-BNR20NM       PIC  N(10).

*集計用領域
 01  WK-SYUKEI-AREA.
     03  WK-S-AREA.
       05  WK-S-AREA2  OCCURS 20. *> サカタ２０分類
         07  WK-S-AREA3  OCCURS 2. *> 1:該当年、2:前年
           09  WK-S-SURYO   PIC S9(13)V99  PACKED-DECIMAL.
           09  WK-S-KINGAK  PIC S9(13)     PACKED-DECIMAL.
           09  WK-S-HEPSU   PIC S9(13)V99  PACKED-DECIMAL.
           09  WK-S-HEPGAK  PIC S9(13)     PACKED-DECIMAL.
     03  WK-G-AREA. *> 仕入先計
       05  WK-G-AREA2  OCCURS 2. *> 1:該当年、2:前年
         07  WK-G-SURYO     PIC S9(13)V99  PACKED-DECIMAL.
         07  WK-G-KINGAK    PIC S9(13)     PACKED-DECIMAL.
         07  WK-G-HEPSU     PIC S9(13)V99  PACKED-DECIMAL.
         07  WK-G-HEPGAK    PIC S9(13)     PACKED-DECIMAL.
     03  WK-T-AREA.  *> 総計
       05  WK-T-AREA2  OCCURS 2. *> 1:該当年、2:前年
         07  WK-T-SURYO     PIC S9(13)V99  PACKED-DECIMAL.
         07  WK-T-KINGAK    PIC S9(13)     PACKED-DECIMAL.
         07  WK-T-HEPSU     PIC S9(13)V99  PACKED-DECIMAL.
         07  WK-T-HEPGAK    PIC S9(13)     PACKED-DECIMAL.
*
****  見出し行１             ****
 01  MIDASI1  CHARACTER TYPE IS  YA.
     02  FILLER             PIC  X(01)  VALUE SPACE.
     02  FILLER             PIC  X(08)  VALUE "SJS0083L".
     02  FILLER             PIC  X(36)  VALUE SPACE.
     02  FILLER             PIC  N(17)  VALUE
         NC"※※　週別仕入先別仕入実績表　※※".
     02  FILLER             PIC  X(35)  VALUE SPACE.
     02  H1-YY              PIC  9(04).
     02  FILLER             PIC  N(01)  VALUE NC"年".
     02  H1-MM              PIC  Z9.
     02  FILLER             PIC  N(01)  VALUE NC"月".
     02  H1-DD              PIC  Z9.
     02  FILLER             PIC  N(01)  VALUE NC"日".
     02  H1-PAGE            PIC  ZZZ9.
     02  FILLER             PIC  N(01)  VALUE NC"頁".

****  見出し行２             ****
 01  MIDASI2  CHARACTER TYPE IS  YA.
     02  FILLER             PIC  X(53)  VALUE  SPACE.
     02  FILLER             PIC  N(01)  VALUE  NC"【".
     02  FILLER             PIC  X(01)  VALUE  SPACE.
     02  H2-NEN             PIC  9(04).
     02  FILLER             PIC  N(01)  VALUE  NC"年".
     02  H2-SYU             PIC  9(02).
     02  FILLER             PIC  N(01)  VALUE  NC"週".
     02  FILLER             PIC  X(01)  VALUE  SPACE.
     02  FILLER             PIC  N(01)  VALUE  NC"】".

****  見出し行３             ****
 01  MIDASI3.
     02  FILLER             PIC  X(01)  VALUE SPACE.
     02  FILLER             PIC  N(05)  VALUE
         NC"仕入先名称"
         CHARACTER TYPE IS  YA.
     02  FILLER             PIC  X(28)  VALUE SPACE.
     02  FILLER             PIC  N(04)  VALUE
         NC"仕入数量"
         CHARACTER TYPE IS  YA.
     02  FILLER             PIC  X(03)  VALUE SPACE.
     02  FILLER             PIC  N(04)  VALUE
         NC"仕入金額"
         CHARACTER TYPE IS  YA.
     02  FILLER             PIC  X(06)  VALUE SPACE.
     02  FILLER             PIC  N(04)  VALUE
         NC"返品数量"
         CHARACTER TYPE IS  YA.
     02  FILLER             PIC  X(03)  VALUE SPACE.
     02  FILLER             PIC  N(04)  VALUE
         NC"返品金額"
         CHARACTER TYPE IS  YA.
     02  FILLER             PIC  X(05)  VALUE SPACE.
     02  FILLER             PIC  N(06)  VALUE
         NC"差引数量合計"
         CHARACTER TYPE IS  YB.
     02  FILLER             PIC  X(02)  VALUE SPACE.
     02  FILLER             PIC  N(06)  VALUE
         NC"差引金額合計"
         CHARACTER TYPE IS  YB.

****  見出し行３－２             ****
 01  MIDASI3-2.
     02  FILLER             PIC  X(01)  VALUE SPACE.
     02  FILLER             PIC  N(04)  VALUE
         NC"分類名称"
         CHARACTER TYPE IS  YA.
     02  FILLER             PIC  X(29)  VALUE SPACE.
     02  FILLER             PIC  N(06)  VALUE
         NC"前年週仕入数"
         CHARACTER TYPE IS  YB.
     02  FILLER             PIC  X(02)  VALUE SPACE.
     02  FILLER             PIC  N(06)  VALUE
         NC"前年週仕入額"
         CHARACTER TYPE IS  YB.
     02  FILLER             PIC  X(05)  VALUE SPACE.
     02  FILLER             PIC  N(06)  VALUE
         NC"前年週返品数"
         CHARACTER TYPE IS  YB.
     02  FILLER             PIC  X(02)  VALUE SPACE.
     02  FILLER             PIC  N(06)  VALUE
         NC"前年週返品額"
         CHARACTER TYPE IS  YB.
     02  FILLER             PIC  X(05)  VALUE SPACE.
     02  FILLER             PIC  N(06)  VALUE
         NC"前年週数量計"
         CHARACTER TYPE IS  YB.
     02  FILLER             PIC  X(02)  VALUE SPACE.
     02  FILLER             PIC  N(06)  VALUE
         NC"前年週金額計"
         CHARACTER TYPE IS  YB.
     02  FILLER             PIC  X(05)  VALUE SPACE.

****  見出し行９             ****
 01  MIDASI9  CHARACTER TYPE IS  YA.
     02  FILLER             PIC  N(68)  VALUE  ALL NC"─".

****  明細行１               ****
 01  MEISAI1  CHARACTER TYPE IS  YB.
     02  FILLER             PIC  X(01).
     02  PRT-TOKCD          PIC  9(08).
     02  FILLER             PIC  N(01).
     02  PRT-TOKNM          PIC  N(15).

****  明細行２               ****
 01  MEISAI2  CHARACTER TYPE IS  YB.
     02  FILLER             PIC  X(01).
     02  PRT2-BNR20         PIC  X(02).
     02  FILLER             PIC  X(06).
     02  FILLER             PIC  N(01).
     02  PRT2-BNR20NM       PIC  N(15).
     02  PRT2-SURYO         PIC  ---,---,--9.99.
     02  PRT2-KINGAK        PIC  ---,---,--9.
     02  PRT2-HEPSU         PIC  ---,---,--9.99.
     02  PRT2-HEPGAK        PIC  ---,---,--9.
     02  PRT2-SA-SU         PIC  ---,---,--9.99.
     02  PRT2-SA-GAK        PIC  ---,---,--9.

****  明細行３               ****
 01  MEISAI3.
     02  FILLER             PIC  X(01).
     02  PRT3-G.
       03  FILLER           PIC  X(02).
       03  FILLER           PIC  X(06).
       03  FILLER           PIC  X(24).
     02  PRT3-SURYO         PIC  ---,---,--9.99.
     02  PRT3-KINGAK        PIC  ---,---,--9.
     02  PRT3-HEPSU         PIC  ---,---,--9.99.
     02  PRT3-HEPGAK        PIC  ---,---,--9.
     02  PRT3-SA-SU         PIC  ---,---,--9.99.
     02  PRT3-SA-GAK        PIC  ---,---,--9.

****  明細行９               ****
 01  MEISAI9.
     02  FILLER  OCCURS 68.
       03  FILLER           PIC  X(02)  VALUE  "- ".
*
*メッセージ情報
 01  MSG-AREA.
     03  MSG-ABEND1.
         05  FILLER          PIC  X(12)  VALUE  "### SJS0083L".
         05  FILLER          PIC  X(11)  VALUE  "  ABEND ###".
     03  MSG-ABEND2.
         05  FILLER          PIC  X(04)  VALUE  "### ".
         05  ERR-FL-ID       PIC  X(08).
         05  FILLER          PIC  X(04)  VALUE  " ST-".
         05  ERR-STCD        PIC  X(02).
         05  FILLER          PIC  X(04)  VALUE  " ###".
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*=============================================================
 LINKAGE             SECTION.
*=============================================================
   01  LINK-SOKCD            PIC  X(02).
   01  LINK-DSOKCD           PIC  X(02).
******************************************************************
 PROCEDURE               DIVISION      USING    LINK-SOKCD
                                                LINK-DSOKCD.
******************************************************************
 DECLARATIVES.
*画面Ｆ
 DSP-ERR-SEC        SECTION.
     USE      AFTER  EXCEPTION   PROCEDURE       DSPF.
     MOVE    "DSPF"        TO    ERR-FL-ID.
     MOVE     DSP-STA      TO    ERR-STCD.
     DISPLAY  MSG-ABEND1   UPON  CONS.
     DISPLAY  MSG-ABEND2   UPON  CONS.
     STOP     RUN.
*実績週集計ファイル
 JSS-ERR-SEC        SECTION.
     USE      AFTER  EXCEPTION   PROCEDURE       JISSSYUF.
     MOVE    "RUISYUL1"    TO    ERR-FL-ID.
     MOVE     JSS-STA      TO    ERR-STCD.
     DISPLAY  MSG-ABEND1   UPON  CONS.
     DISPLAY  MSG-ABEND2   UPON  CONS.
     STOP     RUN.
*実績週集計ファイル（前年）
 JSS-ERR-SEC        SECTION.
     USE      AFTER  EXCEPTION   PROCEDURE       JISSSB1.
     MOVE    "JISSB11"     TO    ERR-FL-ID.
     MOVE     JB1-STA      TO    ERR-STCD.
     DISPLAY  MSG-ABEND1   UPON  CONS.
     DISPLAY  MSG-ABEND2   UPON  CONS.
     STOP     RUN.
*実績週集計ファイル（２年前）
 JSS-ERR-SEC        SECTION.
     USE      AFTER  EXCEPTION   PROCEDURE       JISSSB2.
     MOVE    "JISSB21"     TO    ERR-FL-ID.
     MOVE     JB2-STA      TO    ERR-STCD.
     DISPLAY  MSG-ABEND1   UPON  CONS.
     DISPLAY  MSG-ABEND2   UPON  CONS.
     STOP     RUN.
*実績週集計ファイル（３年前）
 JSS-ERR-SEC        SECTION.
     USE      AFTER  EXCEPTION   PROCEDURE       JISSSB3.
     MOVE    "JISSB31"     TO    ERR-FL-ID.
     MOVE     JB3-STA      TO    ERR-STCD.
     DISPLAY  MSG-ABEND1   UPON  CONS.
     DISPLAY  MSG-ABEND2   UPON  CONS.
     STOP     RUN.
*部門取引先Ｍ
 BUT-ERR-SEC        SECTION.
     USE      AFTER  EXCEPTION   PROCEDURE       BUTOKMF.
     MOVE    "BUTOKMF"     TO    ERR-FL-ID.
     MOVE     BUT-STA      TO    ERR-STCD.
     DISPLAY  MSG-ABEND1   UPON  CONS.
     DISPLAY  MSG-ABEND2   UPON  CONS.
     STOP     RUN.
*条件ファイル
 JYO-ERR-SEC        SECTION.
     USE      AFTER  EXCEPTION   PROCEDURE       HJYOKEN.
     MOVE    "JYOKEN1"     TO    ERR-FL-ID.
     MOVE     JYO-STA      TO    ERR-STCD.
     DISPLAY  MSG-ABEND1   UPON  CONS.
     DISPLAY  MSG-ABEND2   UPON  CONS.
     STOP     RUN.
 END  DECLARATIVES.
*=============================================================
*               コントロール
*=============================================================
 CONTROL-SEC         SECTION.
     DISPLAY  "**  SJS0083L   START  **"   UPON  CONS.
*
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC    UNTIL  MAIN-FLG  =  99.
     PERFORM  END-SEC.
*
     DISPLAY  "**  SJS0083L    END   **"   UPON  CONS.
     STOP  RUN.
 CONTROL-EXIT.
     EXIT.
*=============================================================
*               初期処理
*=============================================================
 INIT-SEC            SECTION.
*ファイル ＯＰＥＮ
     OPEN  I-O    DSPF.
     OPEN  INPUT  JISSSYUF.
     OPEN  INPUT  JISSSB1.
     OPEN  INPUT  JISSSB2.
     OPEN  INPUT  JISSSB3.
     OPEN  INPUT  BUTOKMF.
     OPEN  INPUT  HJYOKEN.
     OPEN  INPUT  HIDUKEF.
     OPEN  OUTPUT PRINTF.
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
*画面表示日付編集
     MOVE      SYS-DATE(1:4)      TO   HEN-DATE-YYYY H1-YY.
     MOVE      SYS-DATE(5:2)      TO   HEN-DATE-MM   H1-MM.
     MOVE      SYS-DATE(7:2)      TO   HEN-DATE-DD   H1-DD.
*システム日付取得
     ACCEPT    WK-TIME          FROM   TIME.
*画面表示時刻編集
     MOVE      WK-TIME(1:2)       TO   HEN-TIME-HH.
     MOVE      WK-TIME(3:2)       TO   HEN-TIME-MM.
     MOVE      WK-TIME(5:2)       TO   HEN-TIME-SS.
*サカタ２０分類テーブル設定
     PERFORM  TB-BNR20-SET-SEC.

*初期画面表示へ
     MOVE  1                TO    MAIN-FLG.
 INIT-EXIT.
     EXIT.
*=============================================================
*  サカタ２０分類テーブル設定処理
*=============================================================
 TB-BNR20-SET-SEC           SECTION.
     INITIALIZE  TB-BNR20-AREA.
     MOVE  ZERO             TO  IX.

     INITIALIZE  JYO-REC.
     MOVE  10               TO  JYO-F01.
     MOVE  LOW-VALUE        TO  FG-HJYOKEN-END.

     PERFORM  HJYOKEN-RD-SEC.
     IF  FG-HJYOKEN-END = "END"
         MOVE  "4000"       TO  PROGRAM-STATUS
         EXIT PROGRAM
     END-IF.

     PERFORM  UNTIL  FG-HJYOKEN-END = "END"
       IF  IX = 20
           MOVE  "4000"     TO  PROGRAM-STATUS
           EXIT PROGRAM
       END-IF

       ADD 1  TO IX
       MOVE  JYO-F02        TO  TB-BNR20   (IX)
       MOVE  JYO-F03        TO  TB-BNR20NM (IX)
       PERFORM  HJYOKEN-RD-SEC

     END-PERFORM.

 TB-BNR20-SET-EXIT.
     EXIT.
*=============================================================
*  サカタ２０分類テーブル設定処理
*=============================================================
 HJYOKEN-RD-SEC             SECTION.
     IF  FG-HJYOKEN-END = LOW-VALUE
         MOVE  SPACE        TO  FG-HJYOKEN-END
         START  HJYOKEN  KEY >= JYO-F01
                                JYO-F02
           INVALID
             MOVE  "END"    TO  FG-HJYOKEN-END
             GO TO  HJYOKEN-RD-EXIT
         END-START
     END-IF.

     READ  HJYOKEN
       AT END
         MOVE  "END"        TO  FG-HJYOKEN-END
         GO TO  HJYOKEN-RD-EXIT
     END-READ.

     IF  JYO-F01 NOT = "10"
         MOVE  "END"        TO  FG-HJYOKEN-END
         GO TO  HJYOKEN-RD-EXIT
     END-IF.

 HJYOKEN-RD-EXIT.
     EXIT.
*=============================================================
*                メイン処理
*=============================================================
 MAIN-SEC            SECTION.
     EVALUATE  MAIN-FLG
*初期画面表示
         WHEN   1      PERFORM  DSP-INIT-SEC
*ＢＯＤＹ部入力
         WHEN   2      PERFORM  DSP-BODY-SEC
*確認入力
         WHEN   3      PERFORM  DSP-KAKU-SEC
*更新処理
         WHEN   4      PERFORM  PRINT-SEC
     END-EVALUATE.
 MAIN-EXIT.
     EXIT.
*=============================================================
*                 終了処理
*=============================================================
 END-SEC             SECTION.
*ファイル ＣＬＯＳＥ
     CLOSE  DSPF.
     CLOSE  JISSSYUF.
     CLOSE  JISSSB1.
     CLOSE  JISSSB2.
     CLOSE  JISSSB3.
     CLOSE  BUTOKMF.
     CLOSE  HJYOKEN.
     CLOSE  HIDUKEF.
     CLOSE  PRINTF.
*
     DISPLAY "* JISSSYUF (IN)=" IN-CNT   " *" UPON CONS.
     DISPLAY "* JISSSB1  (IN)=" IN2-CNT  " *" UPON CONS.
     DISPLAY "* JISSSB2  (IN)=" IN3-CNT  " *" UPON CONS.
     DISPLAY "* JISSSB3  (IN)=" IN4-CNT  " *" UPON CONS.
     DISPLAY "* JISSSYUF (OT)=" OT-CNT   " *" UPON CONS.
     DISPLAY "* JISSSB1  (OT)=" OT2-CNT  " *" UPON CONS.
     DISPLAY "* JISSSB2  (OT)=" OT3-CNT  " *" UPON CONS.
     DISPLAY "* JISSSB3  (OT)=" OT4-CNT  " *" UPON CONS.
     DISPLAY "* PRINTF (PAGE)=" PAGE-CNT " *" UPON CONS.
 END-EXIT.
     EXIT.
*=============================================================
*                画面初期表示処理
*=============================================================
 DSP-INIT-SEC       SECTION.
*初期画面の処理
     MOVE  SPACE            TO    DSP-CONTROL.
     MOVE "FJS00831"        TO    DSP-FMT.
     MOVE  SPACE            TO    DSP-FJS00831.
     PERFORM  OPT-CLR-SEC.
*年月
     MOVE  SYS-DATE         TO  DSP-STYMD.
     MOVE  SYS-DATE         TO  DSP-EDYMD.
*ＢＯＤＹ部入力へ
     MOVE  2                TO    MAIN-FLG.
 DSP-INIT-EXIT.
     EXIT.
*=============================================================
*                項目属性初期化
*=============================================================
 OPT-CLR-SEC        SECTION.
     MOVE  SPACE   TO  EDIT-CURSOR OF DSP-STYMD.
     MOVE  SPACE   TO  EDIT-CURSOR OF DSP-EDYMD.
     MOVE  SPACE   TO  EDIT-CURSOR OF DSP-STTOR.
     MOVE  SPACE   TO  EDIT-CURSOR OF DSP-EDTOR.
     MOVE  SPACE   TO  EDIT-CURSOR OF DSP-BNR20.
     MOVE  "M"     TO  EDIT-OPTION OF DSP-STYMD.
     MOVE  "M"     TO  EDIT-OPTION OF DSP-EDYMD.
     MOVE  "M"     TO  EDIT-OPTION OF DSP-STTOR.
     MOVE  "M"     TO  EDIT-OPTION OF DSP-EDTOR.
     MOVE  "M"     TO  EDIT-OPTION OF DSP-BNR20.

 OPT-CLR-EXIT.
     EXIT.
*=============================================================
*                ＢＯＤＹ部入力処理
*=============================================================
 DSP-BODY-SEC       SECTION.
*画面表示
     MOVE      1        TO  PFK-FLG.
     PERFORM   DSP-WT-SEC.
*画面入力
     MOVE      "BODY"   TO  DSP-GRP.
     PERFORM   DSP-RD-SEC.
*ＰＦ判定
     EVALUATE  DSP-FNC
          WHEN "E000"
                          PERFORM  CHK-BODY-SEC
                          IF    ERR-FLG = ZERO
                                MOVE    3    TO   MAIN-FLG
                          END-IF
          WHEN "F004"
                          MOVE  1      TO  MAIN-FLG
          WHEN "F005"
                          MOVE  99     TO  MAIN-FLG
          WHEN "F006"
                          CONTINUE
          WHEN OTHER
                          MOVE  1      TO  ERR-FLG
     END-EVALUATE.
 DSP-BODY-EXIT.
     EXIT.
*=============================================================
*                ＢＯＤＹ部入力チェック
*=============================================================
 CHK-BODY-SEC   SECTION.

     PERFORM  OPT-CLR-SEC.

*開始年月日
     MOVE  "2"              TO  LINK-IN-KBN.
     MOVE  DSP-STYMD        TO  LINK-IN-YMD8.
     CALL  "SKYDTCKB"  USING LINK-IN-KBN
                             LINK-IN-YMD6
                             LINK-IN-YMD8
                             LINK-OUT-RET
                             LINK-OUT-YMD.
     IF  LINK-OUT-RET NOT = ZERO
         IF  ERR-FLG  =  ZERO
             MOVE  5        TO  ERR-FLG
         END-IF
         MOVE  "R"          TO  EDIT-OPTION OF DSP-STYMD
         MOVE  "C"          TO  EDIT-CURSOR OF DSP-STYMD
         GO TO  CHK-STYMD-EXIT
     END-IF.

     MOVE  LOW-VALUE        TO  FG-HIDUKEF-RVS-END.
     MOVE  DSP-STYMD        TO  JHM-F01. *> 日時更新日付
     PERFORM  HIDUKEF-RD-RVS-SEC.
     IF  FG-HIDUKEF-RVS-END = "END"
         IF  ERR-FLG  =  ZERO
             MOVE  7        TO  ERR-FLG
         END-IF
         MOVE  "R"          TO  EDIT-OPTION OF DSP-STYMD
         MOVE  "C"          TO  EDIT-CURSOR OF DSP-STYMD
         GO TO  CHK-STYMD-EXIT
     END-IF.

     MOVE  JHM-FIL1(1:2)     TO  WK-STSYUNO. *> 開始週番号
     DISPLAY "WK-STSYUNO = " WK-STSYUNO UPON CONS
     DISPLAY "JHM-FIL1(1:2) = " JHM-FIL1(1:2) UPON CONS
 CHK-STYMD-EXIT.
*終了年月日
     MOVE  "2"              TO  LINK-IN-KBN.
     MOVE  DSP-EDYMD        TO  LINK-IN-YMD8.
     CALL  "SKYDTCKB"  USING LINK-IN-KBN
                             LINK-IN-YMD6
                             LINK-IN-YMD8
                             LINK-OUT-RET
                             LINK-OUT-YMD.
     IF  LINK-OUT-RET NOT = ZERO
         IF  ERR-FLG = ZERO
             MOVE  5        TO  ERR-FLG
         END-IF
         MOVE  "R"          TO  EDIT-OPTION OF DSP-EDYMD
         MOVE  "C"          TO  EDIT-CURSOR OF DSP-EDYMD
         GO TO  CHK-EDYMD-EXIT
     END-IF.

     MOVE  LOW-VALUE        TO  FG-HIDUKEF-RVS-END.
     MOVE  DSP-EDYMD        TO  JHM-F01. *> 日時更新日付
     PERFORM  HIDUKEF-RD-RVS-SEC.
     IF  FG-HIDUKEF-RVS-END = "END"
         IF  ERR-FLG  =  ZERO
             MOVE  7        TO  ERR-FLG
         END-IF
         MOVE  "R"          TO  EDIT-OPTION OF DSP-EDYMD
         MOVE  "C"          TO  EDIT-CURSOR OF DSP-EDYMD
         GO TO  CHK-EDYMD-EXIT
     END-IF.

     MOVE  JHM-FIL1(1:2)    TO  WK-EDSYUNO. *> 開始週番号

     IF       ERR-FLG = ZERO
         AND  DSP-STYMD > DSP-EDYMD
         IF  ERR-FLG = ZERO
             MOVE  2        TO  ERR-FLG
         END-IF
         MOVE  "R"          TO  EDIT-OPTION OF DSP-STYMD
         MOVE  "R"          TO  EDIT-OPTION OF DSP-EDYMD
         MOVE  "C"          TO  EDIT-CURSOR OF DSP-STYMD
     END-IF.
     IF       ERR-FLG = ZERO
         AND  DSP-STYMD(1:4) NOT= DSP-EDYMD(1:4)
         IF  ERR-FLG = ZERO
             MOVE  8        TO  ERR-FLG
         END-IF
         MOVE  "R"          TO  EDIT-OPTION OF DSP-STYMD
         MOVE  "R"          TO  EDIT-OPTION OF DSP-EDYMD
         MOVE  "C"          TO  EDIT-CURSOR OF DSP-STYMD
     END-IF.

 CHK-EDYMD-EXIT.
*開始仕入先
 CHK-STTOR-EXIT.
*終了仕入先
     IF  DSP-STTOR NOT NUMERIC
         MOVE  0            TO  DSP-STTOR
     END-IF.

     IF  DSP-EDTOR = ZERO OR SPACE
         MOVE  99999999     TO  DSP-EDTOR
     END-IF.

     IF  DSP-STTOR > DSP-EDTOR
         IF  ERR-FLG = ZERO
             MOVE  2        TO  ERR-FLG
         END-IF
         MOVE  "R"          TO  EDIT-OPTION OF DSP-STTOR
         MOVE  "R"          TO  EDIT-OPTION OF DSP-EDTOR
         MOVE  "C"          TO  EDIT-CURSOR OF DSP-STTOR
     END-IF.

 CHK-EDTOR-EXIT.
*サカタ２０分類
     IF  DSP-BNR20 NOT = SPACE AND "1"
         IF  ERR-FLG = ZERO
             MOVE  6        TO  ERR-FLG
         END-IF
         MOVE  "R"          TO  EDIT-OPTION OF DSP-BNR20
         MOVE  "C"          TO  EDIT-CURSOR OF DSP-BNR20
     END-IF.

 CHK-20BNR-EXIT.

     IF  ERR-FLG NOT = ZERO
         GO TO  CHK-BODY-EXIT
     END-IF.

*対象データ存在ＣＨＫ
     PERFORM  JISSEKI-STRD-SEC.
     IF  FG-JISSEKI-RD-END = "END"
         IF  ERR-FLG = ZERO
             MOVE  3        TO  ERR-FLG
         END-IF
         MOVE  "C"          TO  EDIT-CURSOR OF DSP-STYMD
         MOVE  "R"          TO  EDIT-OPTION OF DSP-STYMD
         MOVE  "R"          TO  EDIT-OPTION OF DSP-EDYMD
         MOVE  "R"          TO  EDIT-OPTION OF DSP-STTOR
         MOVE  "R"          TO  EDIT-OPTION OF DSP-EDTOR
         MOVE  "R"          TO  EDIT-OPTION OF DSP-BNR20
     END-IF.

 CHK-BODY-EXIT.
     EXIT.
*=============================================================
*    日次更新条件ファイル読込み（逆順）
*=============================================================
 HIDUKEF-RD-RVS-SEC           SECTION.
     IF  FG-HIDUKEF-RVS-END = LOW-VALUE
         MOVE  SPACE        TO  FG-HIDUKEF-RVS-END

         START  HIDUKEF  KEY <= JHM-F01 *> 日時更新日付
                WITH REVERSED ORDER
           INVALID
             MOVE  "END"     TO  FG-HIDUKEF-RVS-END
             GO TO  HIDUKEF-RD-RVS-EXIT
         END-START
     END-IF.

     READ  HIDUKEF
       AT END
         MOVE  "END"        TO  FG-HIDUKEF-RVS-END
         GO TO  HIDUKEF-RD-RVS-EXIT
     END-READ.

 HIDUKEF-RD-RVS-EXIT.
     EXIT.
*=============================================================
*    各実績ファイル開始（読み込み）
*=============================================================
 JISSEKI-STRD-SEC           SECTION.

     MOVE  ZERO             TO  IN-CNT.
     MOVE  ZERO             TO  IN2-CNT.
     MOVE  ZERO             TO  IN3-CNT.
     MOVE  ZERO             TO  IN4-CNT.
     MOVE  ZERO             TO  OT-CNT.
     MOVE  ZERO             TO  OT2-CNT.
     MOVE  ZERO             TO  OT3-CNT.
     MOVE  ZERO             TO  OT4-CNT.
     DISPLAY "WK-STSYUNO="  WK-STSYUNO UPON CONS
     MOVE  DSP-STYMD (1:4)  TO  WK-START-NEN.
     MOVE  WK-STSYUNO       TO  WK-START-SYUNO.
     MOVE  DSP-STTOR        TO  WK-START-TOR.

     MOVE  DSP-EDYMD (1:4)  TO  WK-END-NEN.
     MOVE  WK-EDSYUNO       TO  WK-END-SYUNO.
     MOVE  DSP-EDTOR        TO  WK-END-TOR.

     PERFORM  JISSEKI-STRDB-SEC.

 JISSEKI-STRD-EXIT.
     EXIT.
*=============================================================
*    各実績ファイル開始（読み込み）
*=============================================================
 JISSEKI-STRDB-SEC          SECTION.

* 開始実績ファイルを確認するため当年より過去に_り
* 対象データがあるかチェックする。

     MOVE  SPACE            TO  FG-JISSEKI-RD-END.
     MOVE  SPACE            TO  WK-STFILE.
     MOVE  "TOU"            TO  FG-RD-TOUZEN.
     DISPLAY "WK-START-SYUNO=" WK-START-SYUNO  UPON CONS
     COMPUTE  WK-START-NO = HEN-DATE-YYYY - WK-START-NEN.
     IF  WK-START-NO      =  ZERO
       INITIALIZE  JSS-REC
       MOVE  "2"              TO  JSS-F01   *> 売上仕入区分
       MOVE  WK-START-NEN     TO  JSS-F021  *> 年
       MOVE  WK-START-SYUNO   TO  JSS-F022  *> 週番号
       MOVE  WK-START-TOR     TO  JSS-F03   *> 取引先

       MOVE  8                TO  FG-JISSSYUF-END
       PERFORM  JSS-READ-SEC
       IF  FG-JISSSYUF-END  =  ZERO
         MOVE  "JISSSYUF"   TO  WK-STFILE
         GO TO  JISSEKI-STRDB-EXIT
       END-IF
     END-IF

     IF  WK-START-NO      =  1
       INITIALIZE  JB1-REC
       MOVE  "2"              TO  JB1-F01   *> 売上仕入区分
       MOVE  WK-START-NEN     TO  JB1-F021  *> 年
       MOVE  WK-START-SYUNO   TO  JB1-F022  *> 週番号
       MOVE  WK-START-TOR     TO  JB1-F03   *> 取引先

       MOVE  8                TO  FG-JISSSB1-END
       PERFORM  JB1-READ-SEC
       IF  FG-JISSSB1-END  =  ZERO
         MOVE  "JISSSB1 "   TO  WK-STFILE
         GO TO  JISSEKI-STRDB-EXIT
       END-IF
     END-IF

     IF  WK-START-NO      =  2
       INITIALIZE  JB2-REC
       MOVE  "2"              TO  JB2-F01   *> 売上仕入区分
       MOVE  WK-START-NEN     TO  JB2-F021  *> 年
       MOVE  WK-START-SYUNO   TO  JB2-F022  *> 週番号
       MOVE  WK-START-TOR     TO  JB2-F03   *> 取引先

       MOVE  8                TO  FG-JISSSB2-END
       PERFORM  JB2-READ-SEC
       IF  FG-JISSSB2-END  =  ZERO
         MOVE  "JISSSB2 "   TO  WK-STFILE
         GO TO  JISSEKI-STRDB-EXIT
       END-IF
     END-IF.

     IF  WK-START-NO      =  3
       INITIALIZE  JB3-REC
       MOVE  "2"              TO  JB3-F01   *> 売上仕入区分
       MOVE  WK-START-NEN     TO  JB3-F021  *> 年
       MOVE  WK-START-SYUNO   TO  JB3-F022  *> 週番号
       MOVE  WK-START-TOR     TO  JB3-F03   *> 取引先

       MOVE  8                TO  FG-JISSSB3-END
       PERFORM  JB3-READ-SEC
       IF  FG-JISSSB3-END  =  ZERO
         MOVE  "JISSSB3 "   TO  WK-STFILE
         GO TO  JISSEKI-STRDB-EXIT
       END-IF
     END-IF.

     MOVE  "END"            TO  FG-JISSEKI-RD-END.
 JISSEKI-STRDB-EXIT.
     EXIT.
*=============================================================
*                実績週集計ファイル（読み込み）
*=============================================================
 JSS-READ-SEC    SECTION.
        DISPLAY "FG-JISSSYUF-END= " FG-JISSSYUF-END UPON CONS
     IF FG-JISSSYUF-END =  8
        DISPLAY "JSS-F01 = " JSS-F01 UPON CONS
        DISPLAY "JSS-F021 = " JSS-F021 UPON CONS
        DISPLAY "JSS-F022 = " JSS-F022 UPON CONS
        DISPLAY "JSS-F03 = " JSS-F03 UPON CONS
        MOVE  ZERO          TO  FG-JISSSYUF-END

        START  JISSSYUF  KEY >= JSS-F01  *> 売上仕入区分
                                JSS-F021 *> 年
                                JSS-F022 *> 週番号
                                JSS-F03  *> 取引先
                                JSS-F051 *> 商品ＣＤ
                                JSS-F052 *> 品単ＣＤ
                                JSS-F12  *> 相手商品コード
          INVALID
            MOVE  9            TO  FG-JISSSYUF-END
            GO TO  JSS-READ-EXIT
        END-START
     END-IF.

     READ  JISSSYUF NEXT
       AT END
         MOVE  9            TO  FG-JISSSYUF-END
         GO TO  JSS-READ-EXIT
     END-READ.

     ADD  1   TO  IN-CNT.

* 売上仕入区分
     IF  JSS-F01 NOT = "2" *> 仕入
         MOVE  9            TO  FG-JISSSYUF-END
         GO TO  JSS-READ-EXIT
     END-IF.

     IF  FG-RD-TOUZEN = "TOU"
         *> 年
         IF  JSS-F021 > WK-END-NEN
             MOVE  9        TO  FG-JISSSYUF-END
             GO TO  JSS-READ-EXIT
         END-IF
         *> 週番号
         IF  JSS-F022 >  WK-END-SYUNO
             MOVE  9        TO  FG-JISSSYUF-END
             GO TO  JSS-READ-EXIT
         END-IF
         *> 取引先
         IF     JSS-F03 < WK-START-TOR
             OR JSS-F03 > WK-END-TOR
             GO TO  JSS-READ-SEC
         END-IF

         *> 倉庫
         IF  LINK-DSOKCD = "01"
             CONTINUE
         ELSE
             IF  LINK-SOKCD = JSS-F04
                 CONTINUE
             ELSE
                 GO TO  JSS-READ-SEC
             END-IF
         END-IF

         *> キー退避
         MOVE  JSS-F021     TO  INF-F021 *> 年
         MOVE  JSS-F022     TO  INF-F022 *> 週番号
         MOVE  JSS-F03      TO  INF-F03  *> 取引先
     ELSE
         IF  JSS-F021 > WK-ZEN-END-NEN
             MOVE  9        TO  FG-JISSSYUF-END
             GO TO  JSS-READ-EXIT
         END-IF
         *> 週番号
         IF  JSS-F022 >  WK-ZEN-END-SYUNO
             MOVE  9        TO  FG-JISSSYUF-END
             GO TO  JSS-READ-EXIT
         END-IF
         *> 取引先
         IF     JSS-F03 < WK-ZEN-START-TOR
             OR JSS-F03 > WK-ZEN-END-TOR
             GO TO  JSS-READ-SEC
         END-IF

         *> 倉庫
         IF  LINK-DSOKCD = "01"
             CONTINUE
         ELSE
             IF  LINK-SOKCD = JSS-F04
                 CONTINUE
             ELSE
                 GO TO  JSS-READ-SEC
             END-IF
         END-IF

         *> キー退避
         MOVE  JSS-F021     TO  ZEN-INF-F021 *> 年
         MOVE  JSS-F022     TO  ZEN-INF-F022 *> 週番号
         MOVE  JSS-F03      TO  ZEN-INF-F03  *> 取引先
     END-IF.

     ADD  1   TO  OT-CNT.

 JSS-READ-EXIT.
     EXIT.
*=============================================================
*  実績週集計ファイル前年（読み込み）
*=============================================================
 JB1-READ-SEC    SECTION.
     IF FG-JISSSB1-END =  8
        MOVE  ZERO          TO  FG-JISSSB1-END

        START  JISSSB1  KEY >= JB1-F01  *> 売上仕入区分
                               JB1-F021 *> 年
                               JB1-F022 *> 週番号
                               JB1-F03  *> 取引先
                               JB1-F051 *> 商品ＣＤ
                               JB1-F052 *> 品単ＣＤ
                               JB1-F12  *> 相手商品コード
          INVALID
            MOVE  9            TO  FG-JISSSB1-END
            GO TO  JB1-READ-EXIT
        END-START
     END-IF.

     READ  JISSSB1 NEXT
       AT END
         MOVE  9            TO  FG-JISSSB1-END
         GO TO  JB1-READ-EXIT
     END-READ.

     ADD  1   TO  IN2-CNT.

* 売上仕入区分
     IF  JB1-F01 NOT = "2" *> 仕入
         MOVE  9            TO  FG-JISSSB1-END
         GO TO  JB1-READ-EXIT
     END-IF.

     IF  FG-RD-TOUZEN = "TOU"
         *> 年
         IF  JB1-F021 > WK-END-NEN
             MOVE  9        TO  FG-JISSSB1-END
             GO TO  JB1-READ-EXIT
         END-IF
         *> 週番号
         IF  JB1-F022 >  WK-END-SYUNO
             MOVE  9        TO  FG-JISSSB1-END
             GO TO  JB1-READ-EXIT
         END-IF
         *> 取引先
         IF     JB1-F03 < WK-START-TOR
             OR JB1-F03 > WK-END-TOR
             GO TO  JB1-READ-SEC
         END-IF

         *> 倉庫
         IF  LINK-DSOKCD = "01"
             CONTINUE
         ELSE
             IF  LINK-SOKCD = JB1-F04
                 CONTINUE
             ELSE
                 GO TO  JB1-READ-SEC
             END-IF
         END-IF

         *> キー退避
         MOVE  JB1-F021     TO  INF-F021 *> 年
         MOVE  JB1-F022     TO  INF-F022 *> 週番号
         MOVE  JB1-F03      TO  INF-F03  *> 取引先
     ELSE
         *> 年
         IF  JB1-F021 > WK-ZEN-END-NEN
             MOVE  9        TO  FG-JISSSB1-END
             GO TO  JB1-READ-EXIT
         END-IF
         *> 週番号
         IF  JB1-F022 >  WK-ZEN-END-SYUNO
             MOVE  9        TO  FG-JISSSB1-END
             GO TO  JB1-READ-EXIT
         END-IF
         *> 取引先
         IF     JB1-F03 < WK-ZEN-START-TOR
             OR JB1-F03 > WK-ZEN-END-TOR
             GO TO  JB1-READ-SEC
         END-IF

         *> 倉庫
         IF  LINK-DSOKCD = "01"
             CONTINUE
         ELSE
             IF  LINK-SOKCD = JB1-F04
                 CONTINUE
             ELSE
                 GO TO  JB1-READ-SEC
             END-IF
         END-IF

         *> キー退避
         MOVE  JB1-F021     TO  ZEN-INF-F021 *> 年
         MOVE  JB1-F022     TO  ZEN-INF-F022 *> 週番号
         MOVE  JB1-F03      TO  ZEN-INF-F03  *> 取引先
     END-IF.

     ADD  1   TO  OT2-CNT.

 JB1-READ-EXIT.
     EXIT.
*=============================================================
*  実績週集計ファイル２年前（読み込み）
*=============================================================
 JB2-READ-SEC    SECTION.
     IF FG-JISSSB2-END =  8
        MOVE  ZERO          TO  FG-JISSSB2-END

        START  JISSSB2  KEY >= JB2-F01  *> 売上仕入区分
                               JB2-F021 *> 年
                               JB2-F022 *> 週番号
                               JB2-F03  *> 取引先
                               JB2-F051 *> 商品ＣＤ
                               JB2-F052 *> 品単ＣＤ
                               JB2-F12  *> 相手商品コード
          INVALID
            MOVE  9            TO  FG-JISSSB2-END
            GO TO  JB2-READ-EXIT
        END-START
     END-IF.

     READ  JISSSB2 NEXT
       AT END
         MOVE  9            TO  FG-JISSSB2-END
         GO TO  JB2-READ-EXIT
     END-READ.

     ADD  1   TO  IN3-CNT.

* 売上仕入区分
     IF  JB2-F01 NOT = "2" *> 仕入
         MOVE  9            TO  FG-JISSSB2-END
         GO TO  JB2-READ-EXIT
     END-IF.

     IF  FG-RD-TOUZEN = "TOU"
         *> 年
         IF  JB2-F021 > WK-END-NEN
             MOVE  9        TO  FG-JISSSB2-END
             GO TO  JB2-READ-EXIT
         END-IF
         *> 週番号
         IF  JB2-F022 >  WK-END-SYUNO
             MOVE  9        TO  FG-JISSSB2-END
             GO TO  JB2-READ-EXIT
         END-IF
         *> 取引先
         IF     JB2-F03 < WK-START-TOR
             OR JB2-F03 > WK-END-TOR
             GO TO  JB2-READ-SEC
         END-IF

         *> 倉庫
         IF  LINK-DSOKCD = "01"
             CONTINUE
         ELSE
             IF  LINK-SOKCD = JB2-F04
                 CONTINUE
             ELSE
                 GO TO  JB2-READ-SEC
             END-IF
         END-IF

         *> キー退避
         MOVE  JB2-F021     TO  INF-F021 *> 年
         MOVE  JB2-F022     TO  INF-F022 *> 週番号
         MOVE  JB2-F03      TO  INF-F03  *> 取引先
     ELSE
         *> 年
         IF  JB2-F021 > WK-ZEN-END-NEN
             MOVE  9        TO  FG-JISSSB2-END
             GO TO  JB2-READ-EXIT
         END-IF
         *> 週番号
         IF  JB2-F022 >  WK-ZEN-END-SYUNO
             MOVE  9        TO  FG-JISSSB2-END
             GO TO  JB2-READ-EXIT
         END-IF
         *> 取引先
         IF     JB2-F03 < WK-ZEN-START-TOR
             OR JB2-F03 > WK-ZEN-END-TOR
             GO TO  JB2-READ-SEC
         END-IF

         *> 倉庫
         IF  LINK-DSOKCD = "01"
             CONTINUE
         ELSE
             IF  LINK-SOKCD = JB2-F04
                 CONTINUE
             ELSE
                 GO TO  JB2-READ-SEC
             END-IF
         END-IF

         *> キー退避
         MOVE  JB2-F021     TO  ZEN-INF-F021 *> 年
         MOVE  JB2-F022     TO  ZEN-INF-F022 *> 週番号
         MOVE  JB2-F03      TO  ZEN-INF-F03  *> 取引先
     END-IF.

     ADD  1   TO  OT3-CNT.

 JB2-READ-EXIT.
     EXIT.
*=============================================================
*  実績週集計ファイル３年前（読み込み）
*=============================================================
 JB3-READ-SEC    SECTION.
     IF FG-JISSSB3-END =  8
        MOVE  ZERO          TO  FG-JISSSB3-END

        START  JISSSB3  KEY >= JB3-F01  *> 売上仕入区分
                               JB3-F021 *> 年
                               JB3-F022 *> 週番号
                               JB3-F03  *> 取引先
                               JB3-F051 *> 商品ＣＤ
                               JB3-F052 *> 品単ＣＤ
                               JB3-F12  *> 相手商品コード
          INVALID
            MOVE  9            TO  FG-JISSSB3-END
            GO TO  JB3-READ-EXIT
        END-START
     END-IF.

     READ  JISSSB3 NEXT
       AT END
         MOVE  9            TO  FG-JISSSB3-END
         GO TO  JB3-READ-EXIT
     END-READ.

     ADD  1   TO  IN4-CNT.

* 売上仕入区分
     IF  JB3-F01 NOT = "2" *> 仕入
         MOVE  9            TO  FG-JISSSB3-END
         GO TO  JB3-READ-EXIT
     END-IF.

     IF  FG-RD-TOUZEN = "TOU"
         *> 年
         IF  JB3-F021 > WK-END-NEN
             MOVE  9        TO  FG-JISSSB3-END
             GO TO  JB3-READ-EXIT
         END-IF
         *> 週番号
         IF  JB3-F022 >  WK-END-SYUNO
             MOVE  9        TO  FG-JISSSB3-END
             GO TO  JB3-READ-EXIT
         END-IF
         *> 取引先
         IF     JB3-F03 < WK-START-TOR
             OR JB3-F03 > WK-END-TOR
             GO TO  JB3-READ-SEC
         END-IF

         *> 倉庫
         IF  LINK-DSOKCD = "01"
             CONTINUE
         ELSE
             IF  LINK-SOKCD = JB3-F04
                 CONTINUE
             ELSE
                 GO TO  JB3-READ-SEC
             END-IF
         END-IF

         *> キー退避
         MOVE  JB3-F021     TO  INF-F021 *> 年
         MOVE  JB3-F022     TO  INF-F022 *> 週番号
         MOVE  JB3-F03      TO  INF-F03  *> 取引先
     ELSE
         *> 年
         IF  JB3-F021 > WK-ZEN-END-NEN
             MOVE  9        TO  FG-JISSSB3-END
             GO TO  JB3-READ-EXIT
         END-IF
         *> 週番号
         IF  JB3-F022 >  WK-ZEN-END-SYUNO
             MOVE  9        TO  FG-JISSSB3-END
             GO TO  JB3-READ-EXIT
         END-IF
         *> 取引先
         IF     JB3-F03 < WK-ZEN-START-TOR
             OR JB3-F03 > WK-ZEN-END-TOR
             GO TO  JB3-READ-SEC
         END-IF

         *> 倉庫
         IF  LINK-DSOKCD = "01"
             CONTINUE
         ELSE
             IF  LINK-SOKCD = JB3-F04
                 CONTINUE
             ELSE
                 GO TO  JB3-READ-SEC
             END-IF
         END-IF

         *> キー退避
         MOVE  JB3-F021     TO  ZEN-INF-F021 *> 年
         MOVE  JB3-F022     TO  ZEN-INF-F022 *> 週番号
         MOVE  JB3-F03      TO  ZEN-INF-F03  *> 取引先
     END-IF.

     ADD  1   TO  OT4-CNT.

 JB3-READ-EXIT.
     EXIT.
*=============================================================
*    各実績ファイル終了（読み込み）
*=============================================================
 JISSEKI-EDRD-SEC           SECTION.

     MOVE  ZERO             TO  IN-RVS-CNT.
     MOVE  ZERO             TO  IN2-RVS-CNT.
     MOVE  ZERO             TO  IN3-RVS-CNT.
     MOVE  ZERO             TO  IN4-RVS-CNT.
     MOVE  ZERO             TO  OT-RVS-CNT.
     MOVE  ZERO             TO  OT2-RVS-CNT.
     MOVE  ZERO             TO  OT3-RVS-CNT.
     MOVE  ZERO             TO  OT4-RVS-CNT.

     MOVE  DSP-STYMD (1:4)  TO  WK-RVS-START-NEN.
     MOVE  WK-STSYUNO       TO  WK-RVS-START-SYUNO.
     MOVE  DSP-STTOR        TO  WK-RVS-START-TOR.

     MOVE  DSP-EDYMD (1:4)  TO  WK-RVS-END-NEN.
     MOVE  WK-STSYUNO       TO  WK-RVS-END-SYUNO.
     MOVE  DSP-EDTOR        TO  WK-RVS-END-TOR.

     PERFORM  JISSEKI-EDRDB-SEC.

 JISSEKI-EDRD-EXIT.
     EXIT.
*=============================================================
*    各実績ファイル終了（読み込み）
*=============================================================
 JISSEKI-EDRDB-SEC          SECTION.

* 終了実績ファイルを確認するため３年前より現在に_り
* 対象データがあるかチェックする。

     MOVE  SPACE            TO  FG-JISSEKI-RD-RVS-END.
     MOVE  SPACE            TO  WK-EDFILE.
     MOVE  "ZEN"            TO  FG-RD-TOUZEN.

     INITIALIZE  JB3-REC.
     MOVE  "2"              TO  JB3-F01.  *> 売上仕入区分
     MOVE  WK-RVS-END-NEN   TO  JB3-F021. *> 年
     MOVE  WK-RVS-END-SYUNO TO  JB3-F022  *> 週番号
     MOVE  WK-RVS-END-TOR   TO  JB3-F03.  *> 取引先
     MOVE  HIGH-VALUE       TO  JB3-F051. *> 商品ＣＤ
     MOVE  HIGH-VALUE       TO  JB3-F052. *> 品単ＣＤ
     MOVE  HIGH-VALUE       TO  JB3-F12   *> 相手商品コード

     MOVE  8                TO  FG-JISSSB3-RVS-END.
     PERFORM  RVS-JB3-READ-SEC.
     IF  FG-JISSSB3-RVS-END  =  ZERO
         MOVE  "JISSSB3 "   TO  WK-EDFILE
         GO TO  JISSEKI-EDRDB-EXIT
     END-IF.

     INITIALIZE  JB2-REC.
     MOVE  "2"              TO  JB2-F01.  *> 売上仕入区分
     MOVE  WK-RVS-END-NEN   TO  JB2-F021. *> 年
     MOVE  WK-RVS-END-SYUNO TO  JB2-F022  *> 週番号
     MOVE  WK-RVS-END-TOR   TO  JB2-F03.  *> 取引先
     MOVE  HIGH-VALUE       TO  JB2-F051. *> 商品ＣＤ
     MOVE  HIGH-VALUE       TO  JB2-F052. *> 品単ＣＤ
     MOVE  HIGH-VALUE       TO  JB2-F12   *> 相手商品コード

     MOVE  8                TO  FG-JISSSB2-RVS-END.
     PERFORM  RVS-JB2-READ-SEC.
     IF  FG-JISSSB2-RVS-END  =  ZERO
         MOVE  "JISSSB2 "   TO  WK-EDFILE
         GO TO  JISSEKI-EDRDB-EXIT
     END-IF.

     INITIALIZE  JB1-REC.
     MOVE  "2"              TO  JB1-F01.  *> 売上仕入区分
     MOVE  WK-RVS-END-NEN   TO  JB1-F021. *> 年
     MOVE  WK-RVS-END-SYUNO TO  JB1-F022  *> 週番号
     MOVE  WK-RVS-END-TOR   TO  JB1-F03.  *> 取引先
     MOVE  HIGH-VALUE       TO  JB1-F051. *> 商品ＣＤ
     MOVE  HIGH-VALUE       TO  JB1-F052. *> 品単ＣＤ
     MOVE  HIGH-VALUE       TO  JB1-F12   *> 相手商品コード

     MOVE  8                TO  FG-JISSSB1-RVS-END.
     PERFORM  RVS-JB1-READ-SEC.
     IF  FG-JISSSB1-RVS-END  =  ZERO
         MOVE  "JISSSB1 "   TO  WK-EDFILE
         GO TO  JISSEKI-EDRDB-EXIT
     END-IF.

     INITIALIZE  JSS-REC.
     MOVE  "2"              TO  JSS-F01.  *> 売上仕入区分
     MOVE  WK-RVS-END-NEN   TO  JSS-F021. *> 年
     MOVE  WK-RVS-END-SYUNO TO  JSS-F022  *> 週番号
     MOVE  WK-RVS-END-TOR   TO  JSS-F03.  *> 取引先
     MOVE  HIGH-VALUE       TO  JSS-F051. *> 商品ＣＤ
     MOVE  HIGH-VALUE       TO  JSS-F052. *> 品単ＣＤ
     MOVE  HIGH-VALUE       TO  JSS-F12   *> 相手商品コード

     MOVE  8                TO  FG-JISSSYUF-RVS-END.

     PERFORM  RVS-JSS-READ-SEC.
     IF  FG-JISSSYUF-RVS-END  =  ZERO
         MOVE  "JISSSYUF"   TO  WK-EDFILE
         GO TO  JISSEKI-EDRDB-EXIT
     END-IF.

     MOVE  "END"            TO  FG-JISSEKI-RD-RVS-END.
 JISSEKI-EDRDB-EXIT.
     EXIT.
*=============================================================
*  実績週集計ファイル３年前（逆順読み込み）
*=============================================================
 RVS-JB3-READ-SEC    SECTION.
     IF FG-JISSSB3-RVS-END =  8
        MOVE  ZERO          TO  FG-JISSSB3-RVS-END

        START  JISSSB3  KEY <= JB3-F01  *> 売上仕入区分
                               JB3-F021 *> 年
                               JB3-F022 *> 週番号
                               JB3-F03  *> 取引先
                               JB3-F051 *> 商品ＣＤ
                               JB3-F052 *> 品単ＣＤ
                               JB3-F12  *> 相手商品コード
               WITH REVERSED ORDER
          INVALID
            MOVE  9            TO  FG-JISSSB3-RVS-END
            GO TO  RVS-JB3-READ-EXIT
        END-START
     END-IF.

     READ  JISSSB3 NEXT
       AT END
         MOVE  9            TO  FG-JISSSB3-RVS-END
         GO TO  RVS-JB3-READ-EXIT
     END-READ.

     ADD  1   TO  IN-RVS-CNT.

* 売上仕入区分
     IF  JB3-F01 NOT = "2" *> 仕入
         MOVE  9            TO  FG-JISSSB3-RVS-END
         GO TO  RVS-JB3-READ-EXIT
     END-IF.
* 年
     IF  JB3-F021 < WK-RVS-START-NEN
         MOVE  9            TO  FG-JISSSB3-RVS-END
         GO TO  RVS-JB3-READ-EXIT
     END-IF.
* 週番号
     IF  JB3-F022 < WK-RVS-START-SYUNO
         MOVE  9            TO  FG-JISSSB3-RVS-END
         GO TO  RVS-JB3-READ-EXIT
     END-IF.
* 取引先
     IF     JB3-F03 < WK-RVS-START-TOR
         OR JB3-F03 > WK-RVS-END-TOR
         GO TO  RVS-JB3-READ-SEC
     END-IF.

*倉庫
     IF  LINK-DSOKCD = "01"
         CONTINUE
     ELSE
         IF  LINK-SOKCD = JB3-F04
             CONTINUE
         ELSE
             GO TO  RVS-JB3-READ-SEC
         END-IF
     END-IF.

     ADD  1   TO  OT-RVS-CNT.

 RVS-JB3-READ-EXIT.
     EXIT.
*=============================================================
*  実績週集計ファイル２年前（逆順読み込み）
*=============================================================
 RVS-JB2-READ-SEC    SECTION.
     IF FG-JISSSB2-RVS-END =  8
        MOVE  ZERO          TO  FG-JISSSB2-RVS-END

        START  JISSSB2  KEY <= JB2-F01  *> 売上仕入区分
                               JB2-F021 *> 年
                               JB2-F022 *> 週番号
                               JB2-F03  *> 取引先
                               JB2-F051 *> 商品ＣＤ
                               JB2-F052 *> 品単ＣＤ
                               JB2-F12  *> 相手商品コード
               WITH REVERSED ORDER
          INVALID
            MOVE  9            TO  FG-JISSSB2-RVS-END
            GO TO  RVS-JB2-READ-EXIT
        END-START
     END-IF.

     READ  JISSSB2 NEXT
       AT END
         MOVE  9            TO  FG-JISSSB2-RVS-END
         GO TO  RVS-JB2-READ-EXIT
     END-READ.

     ADD  1   TO  IN2-RVS-CNT.

* 売上仕入区分
     IF  JB2-F01 NOT = "2" *> 仕入
         MOVE  9            TO  FG-JISSSB2-RVS-END
         GO TO  RVS-JB2-READ-EXIT
     END-IF.
* 年
     IF  JB2-F021 < WK-RVS-START-NEN
         MOVE  9            TO  FG-JISSSB2-RVS-END
         GO TO  RVS-JB2-READ-EXIT
     END-IF.
* 週番号
     IF  JB2-F022 <  WK-RVS-START-SYUNO
         MOVE  9            TO  FG-JISSSB2-RVS-END
         GO TO  RVS-JB2-READ-EXIT
     END-IF.
* 取引先
     IF     JB2-F03 < WK-RVS-START-TOR
         OR JB2-F03 > WK-RVS-END-TOR
         GO TO  RVS-JB2-READ-SEC
     END-IF.

* 倉庫
     IF  LINK-DSOKCD = "01"
         CONTINUE
     ELSE
         IF  LINK-SOKCD = JB2-F04
             CONTINUE
         ELSE
             GO TO  RVS-JB2-READ-SEC
         END-IF
     END-IF.

     ADD  1   TO  OT2-RVS-CNT.

 RVS-JB2-READ-EXIT.
     EXIT.
*=============================================================
*  実績週集計ファイル前年（逆順読み込み）
*=============================================================
 RVS-JB1-READ-SEC    SECTION.
     IF FG-JISSSB1-RVS-END =  8
        MOVE  ZERO          TO  FG-JISSSB1-RVS-END

        START  JISSSB1  KEY <= JB1-F01  *> 売上仕入区分
                               JB1-F021 *> 年
                               JB1-F022 *> 週番号
                               JB1-F03  *> 取引先
                               JB1-F051 *> 商品ＣＤ
                               JB1-F052 *> 品単ＣＤ
                               JB1-F12  *> 相手商品コード
               WITH REVERSED ORDER
          INVALID
            MOVE  9            TO  FG-JISSSB1-RVS-END
            GO TO  RVS-JB1-READ-EXIT
        END-START
     END-IF.

     READ  JISSSB1 NEXT
       AT END
         MOVE  9            TO  FG-JISSSB1-RVS-END
         GO TO  RVS-JB1-READ-EXIT
     END-READ.

     ADD  1   TO  IN3-RVS-CNT.

* 売上仕入区分
     IF  JB1-F01 NOT = "2" *> 仕入
         MOVE  9            TO  FG-JISSSB1-RVS-END
         GO TO  RVS-JB1-READ-EXIT
     END-IF.
* 年
     IF  JB1-F021 < WK-RVS-START-NEN
         MOVE  9            TO  FG-JISSSB1-RVS-END
         GO TO  RVS-JB1-READ-EXIT
     END-IF.
* 週番号
     IF  JB1-F022 <  WK-RVS-START-SYUNO
         MOVE  9            TO  FG-JISSSB1-RVS-END
         GO TO  RVS-JB1-READ-EXIT
     END-IF.
* 取引先
     IF     JB1-F03 < WK-RVS-START-TOR
         OR JB1-F03 > WK-RVS-END-TOR
         GO TO  RVS-JB1-READ-SEC
     END-IF.

* 倉庫
     IF  LINK-DSOKCD = "01"
         CONTINUE
     ELSE
         IF  LINK-SOKCD = JB1-F04
             CONTINUE
         ELSE
             GO TO  RVS-JB1-READ-SEC
         END-IF
     END-IF.

     ADD  1   TO  OT3-RVS-CNT.

 RVS-JB1-READ-EXIT.
     EXIT.
*=============================================================
*                実績週集計ファイル（逆順読み込み）
*=============================================================
 RVS-JSS-READ-SEC    SECTION.
     IF FG-JISSSYUF-RVS-END =  8
        MOVE  ZERO          TO  FG-JISSSYUF-RVS-END

        START  JISSSYUF  KEY <= JSS-F01  *> 売上仕入区分
                                JSS-F021 *> 年
                                JSS-F022 *> 週番号
                                JSS-F03  *> 取引先
                                JSS-F051 *> 商品ＣＤ
                                JSS-F052 *> 品単ＣＤ
                                JSS-F12  *> 相手商品コード
               WITH REVERSED ORDER
          INVALID
            MOVE  9            TO  FG-JISSSYUF-RVS-END
            GO TO  RVS-JSS-READ-EXIT
        END-START
     END-IF.

     READ  JISSSYUF NEXT
       AT END
         MOVE  9            TO  FG-JISSSYUF-RVS-END
         GO TO  RVS-JSS-READ-EXIT
     END-READ.

     ADD  1   TO  IN4-RVS-CNT.

* 売上仕入区分
     IF  JSS-F01 NOT = "2" *> 仕入
         MOVE  9            TO  FG-JISSSYUF-RVS-END
         GO TO  RVS-JSS-READ-EXIT
     END-IF.
* 年
     IF  JSS-F021 < WK-RVS-START-NEN
         MOVE  9            TO  FG-JISSSYUF-RVS-END
         GO TO  RVS-JSS-READ-EXIT
     END-IF.
* 週番号
     IF  JSS-F022 < WK-RVS-START-SYUNO
         MOVE  9            TO  FG-JISSSYUF-RVS-END
         GO TO  RVS-JSS-READ-EXIT
     END-IF.
* 取引先
     IF     JSS-F03 < WK-RVS-START-TOR
         OR JSS-F03 > WK-RVS-END-TOR
         GO TO  RVS-JSS-READ-SEC
     END-IF.

* 倉庫
     IF  LINK-DSOKCD = "01"
         CONTINUE
     ELSE
         IF  LINK-SOKCD = JSS-F04
             CONTINUE
         ELSE
             GO TO  RVS-JSS-READ-SEC
         END-IF
     END-IF.

     ADD  1   TO  OT4-RVS-CNT.

 RVS-JSS-READ-EXIT.
     EXIT.
*=============================================================
*                確認入力処理
*=============================================================
 DSP-KAKU-SEC       SECTION.
*画面表示
     MOVE  2                    TO  PFK-FLG.
     PERFORM  DSP-WT-SEC.
*画面入力
     MOVE     "TAIL"            TO  DSP-GRP.
     PERFORM  DSP-RD-SEC.
*ＰＦ判定
     EVALUATE  DSP-FNC
         WHEN "E000"
                          PERFORM  CHK-KAKU-SEC
         WHEN "F004"
                          MOVE  1       TO   MAIN-FLG
         WHEN "F005"
                          MOVE  99      TO   MAIN-FLG
         WHEN "F006"
                          MOVE  2       TO   MAIN-FLG
         WHEN OTHER
                          MOVE  1       TO   ERR-FLG
     END-EVALUATE.
*
 DSP-KAKU-EXIT.
     EXIT.
*=============================================================
*                確認入力チェック（対象データ存在ＣＨＫ）
*=============================================================
 CHK-KAKU-SEC   SECTION.
     MOVE  ZERO             TO  ERR-FLG.
     MOVE  SPACE            TO  WK-STFILE-TOU.
     MOVE  SPACE            TO  WK-EDFILE-TOU.
     MOVE  SPACE            TO  WK-STFILE-ZEN.
     MOVE  SPACE            TO  WK-EDFILE-ZEN.

     PERFORM  JISSEKI-STRD-SEC.
     MOVE  WK-STFILE        TO  WK-STFILE-TOU.
*対象データ存在ＣＨＫ
     IF  FG-JISSEKI-RD-END = "END"
         MOVE  3            TO  ERR-FLG
     END-IF.

*    PERFORM  JISSEKI-EDRD-SEC.
*    MOVE  WK-EDFILE        TO  WK-EDFILE-TOU.

     IF  ERR-FLG = ZERO
         MOVE  4            TO  ERR-FLG
         PERFORM  DSP-WT-SEC
         MOVE  4            TO  MAIN-FLG
         INITIALIZE  WK-SYUKEI-AREA
     ELSE
         MOVE  99           TO  MAIN-FLG
     END-IF.

 CHK-KAKU-EXIT.
     EXIT.
*=============================================================
*                画面表示処理
*=============================================================
 DSP-WT-SEC       SECTION.
*ＰＦキー設定
     MOVE  TBL-PFK(PFK-FLG)      TO  DSP-PFKEY.
*エラー設定
     IF    ERR-FLG   NOT = ZERO
       MOVE  TBL-MSG(ERR-FLG)    TO  DSP-ERRMSG
     END-IF.
     MOVE     HEN-DATE           TO  DSP-SDATE.
     MOVE     HEN-TIME           TO  DSP-STIME.
*画面表示
     MOVE "SCREEN"               TO  DSP-GRP.
     MOVE  SPACE                 TO  DSP-PRO.
     WRITE DSP-FJS00831.
*エラークリア
     MOVE  ZERO                  TO  ERR-FLG.
     MOVE  SPACE                 TO  DSP-ERRMSG.
 DSP-WT-EXIT.
     EXIT.
*=============================================================
*                画面読込処理
*=============================================================
 DSP-RD-SEC        SECTION.
     MOVE "NE"              TO  DSP-PRO.
     READ  DSPF.
     MOVE  ZERO             TO  ERR-FLG.
     PERFORM       OPT-CLR-SEC.
 DSP-RD-EXIT.
     EXIT.
*=============================================================
*                帳票出力
*=============================================================
 PRINT-SEC           SECTION.

     INITIALIZE  WK-T-AREA.
     MOVE  66               TO  LINE-CNT.
* 開始検索ファイルを開始ファイルインデックスに変換。
     EVALUATE  TRUE
       WHEN  WK-STFILE-TOU = "JISSSYUF"
         MOVE  ZERO         TO  FG-STFILE
       WHEN  WK-STFILE-TOU = "JISSSB1 "
         MOVE  1            TO  FG-STFILE
       WHEN  WK-STFILE-TOU = "JISSSB2 "
         MOVE  2            TO  FG-STFILE
       WHEN  WK-STFILE-TOU = "JISSSB3 "
         MOVE  3            TO  FG-STFILE
     END-EVALUATE.

* 終了検索ファイルを終了ファイルインデックスに変換。
     EVALUATE  TRUE
       WHEN  WK-STFILE-TOU = "JISSSYUF"
         MOVE  ZERO         TO  FG-EDFILE
       WHEN  WK-STFILE-TOU = "JISSSB1 "
         MOVE  1            TO  FG-EDFILE
       WHEN  WK-STFILE-TOU = "JISSSB2 "
         MOVE  2            TO  FG-EDFILE
       WHEN  WK-STFILE-TOU = "JISSSB3 "
         MOVE  3            TO  FG-EDFILE
     END-EVALUATE.

* 開始検索ファイルから終了検索ファイルまで印刷処理を行う。

     PERFORM  VARYING IX-FIL  FROM FG-STFILE BY 1
              UNTIL   IX-FIL  > FG-EDFILE

       PERFORM  PRINTB-SEC

     END-PERFORM.

     IF PAGE-CNT NOT = ZERO *> 総計
        PERFORM  SOKEI-PRINT-SEC
     END-IF.

     MOVE  99               TO  MAIN-FLG.

 PRINT-EXIT.
     EXIT.
*=============================================================
*  帳票出力Ｂ
*=============================================================
 PRINTB-SEC           SECTION.
     PERFORM  PRINTC-MAE-SEC. *> 前処理

     PERFORM  UNTIL FG-JISSEKI-RD-END = "END"
       PERFORM  PRINTC-SEC

     END-PERFORM.

 PRINTB-EXIT.
     EXIT.
*=============================================================
*  帳票出力前処理Ｃ
*=============================================================
 PRINTC-MAE-SEC             SECTION.

* 当年
     MOVE  SPACE            TO  FG-JISSEKI-RD-END.
     MOVE  DSP-STYMD (1:4)  TO  WK-START-NEN.
     MOVE  WK-STSYUNO       TO  WK-START-SYUNO.
     MOVE  DSP-STTOR        TO  WK-START-TOR.

     MOVE  DSP-EDYMD (1:4)  TO  WK-END-NEN.
     MOVE  WK-EDSYUNO       TO  WK-END-SYUNO.
     MOVE  DSP-EDTOR        TO  WK-END-TOR.

     EVALUATE  TRUE
       WHEN  IX-FIL = ZERO  *> 実績週集計ファイル
         INITIALIZE  JSS-REC
         MOVE  "2"               TO  JSS-F01  *> 売上仕入区分
         MOVE  WK-START-NEN      TO  JSS-F021 *> 年
         MOVE  WK-START-SYUNO    TO  JSS-F022 *> 週番号
         MOVE  WK-START-TOR      TO  JSS-F03  *> 取引先

         MOVE  8                 TO  FG-JISSSYUF-END

       WHEN  IX-FIL = 1  *> 実績週集計退避１ファイル
         INITIALIZE  JB1-REC
         MOVE  "2"               TO  JB1-F01  *> 売上仕入区分
         MOVE  WK-START-NEN      TO  JB1-F021 *> 年
         MOVE  WK-START-SYUNO    TO  JB1-F022 *> 週番号
         MOVE  WK-START-TOR      TO  JB1-F03  *> 取引先

         MOVE  8                 TO  FG-JISSSB1-END

       WHEN  IX-FIL = 2  *> 実績週集計退避２ファイル
         INITIALIZE  JB2-REC
         MOVE  "2"               TO  JB2-F01  *> 売上仕入区分
         MOVE  WK-START-NEN      TO  JB2-F021 *> 年
         MOVE  WK-START-SYUNO    TO  JB2-F022 *> 週番号
         MOVE  WK-START-TOR      TO  JB2-F03  *> 取引先

         MOVE  8                 TO  FG-JISSSB2-END

       WHEN  IX-FIL = 3  *> 実績週集計退避３ファイル
         INITIALIZE  JB3-REC
         MOVE  "2"               TO  JB3-F01  *> 売上仕入区分
         MOVE  WK-START-NEN      TO  JB3-F021 *> 年
         MOVE  WK-START-SYUNO    TO  JB3-F022 *> 週番号
         MOVE  WK-START-TOR      TO  JB3-F03  *> 取引先

         MOVE  8                 TO  FG-JISSSB3-END

     END-EVALUATE.

     PERFORM  JISSEKI-RD-SEC.
     IF  FG-JISSEKI-RD-END = "END"
         MOVE  3            TO  ERR-FLG
     END-IF.

* 前年
     MOVE  SPACE            TO  FG-ZEN-JISSEKI-RD-END.

     MOVE  DSP-STYMD (1:4)  TO  WK-ZEN-START-NEN.
     COMPUTE  WK-ZEN-START-NEN = WK-ZEN-START-NEN - 1.
     MOVE  WK-STSYUNO       TO  WK-ZEN-START-SYUNO.
     MOVE  DSP-STTOR        TO  WK-ZEN-START-TOR.

     MOVE  DSP-EDYMD (1:4)  TO  WK-ZEN-END-NEN.
     COMPUTE  WK-ZEN-END-NEN = WK-ZEN-END-NEN - 1.
     MOVE  WK-EDSYUNO       TO  WK-ZEN-END-SYUNO.
     MOVE  DSP-EDTOR        TO  WK-ZEN-END-TOR.

     EVALUATE  TRUE
       WHEN  IX-FIL = ZERO  *> 実績週集計ファイル基準
         INITIALIZE  JB1-REC
         MOVE  "2"                 TO  JB1-F01  *> 売上仕入区分
         MOVE  WK-ZEN-START-NEN    TO  JB1-F021 *> 年
         MOVE  WK-ZEN-START-SYUNO  TO  JB1-F022 *> 週番号
         MOVE  WK-ZEN-START-TOR    TO  JB1-F03  *> 取引先

         MOVE  8            TO  FG-JISSSB1-END

       WHEN  IX-FIL = 1  *> 実績週集計退避２ファイル基準
         INITIALIZE  JB2-REC
         MOVE  "2"                 TO  JB2-F01  *> 売上仕入区分
         MOVE  WK-ZEN-START-NEN    TO  JB2-F021 *> 年
         MOVE  WK-ZEN-START-SYUNO  TO  JB2-F022 *> 週番号
         MOVE  WK-ZEN-START-TOR    TO  JB2-F03  *> 取引先

         MOVE  8            TO  FG-JISSSB2-END

       WHEN  IX-FIL = 2  *> 実績週集計退避３ファイル基準
         INITIALIZE  JB3-REC
         MOVE  "2"                 TO  JB3-F01  *> 売上仕入区分
         MOVE  WK-ZEN-START-NEN    TO  JB3-F021 *> 年
         MOVE  WK-ZEN-START-SYUNO  TO  JB3-F022 *> 週番号
         MOVE  WK-ZEN-START-TOR    TO  JB3-F03  *> 取引先

         MOVE  8            TO  FG-JISSSB3-END

     END-EVALUATE.

     PERFORM  ZEN-JISSEKI-RD-SEC.

 PRINTC-MAE-EXIT.
     EXIT.
*=============================================================
*  実績ファイル系読込み
*=============================================================
 JISSEKI-RD-SEC             SECTION.

     MOVE  "TOU"            TO  FG-RD-TOUZEN.

     EVALUATE  TRUE
       WHEN  IX-FIL = ZERO  *> 実績週集計ファイル
         PERFORM  JSS-READ-SEC
         IF  FG-JISSSYUF-END = 9
             MOVE  "END"    TO  FG-JISSEKI-RD-END
         END-IF

       WHEN  IX-FIL = 1  *> 実績週集計退避１ファイル
         PERFORM  JB1-READ-SEC
         IF  FG-JISSSB1-END = 9
             MOVE  "END"    TO  FG-JISSEKI-RD-END
         END-IF

       WHEN  IX-FIL = 2  *> 実績週集計退避２ファイル
         PERFORM  JB2-READ-SEC
         IF  FG-JISSSB2-END = 9
             MOVE  "END"    TO  FG-JISSEKI-RD-END
         END-IF

       WHEN  IX-FIL = 3  *> 実績週集計退避３ファイル
         PERFORM  JB3-READ-SEC
         IF  FG-JISSSB3-END = 9
             MOVE  "END"    TO  FG-JISSEKI-RD-END
         END-IF

     END-EVALUATE.

 JISSEKI-RD-EXIT.
     EXIT.
*=============================================================
*  前年同週実績ファイル系読込み
*=============================================================
 ZEN-JISSEKI-RD-SEC         SECTION.

     MOVE  "ZEN"            TO  FG-RD-TOUZEN.

     EVALUATE  TRUE
       WHEN  IX-FIL = ZERO  *> 実績週集計ファイル基準
         PERFORM  JB1-READ-SEC
         IF  FG-JISSSB1-END = 9
             MOVE  "END"    TO  FG-ZEN-JISSEKI-RD-END
         END-IF

       WHEN  IX-FIL = 1  *> 実績週集計退避１ファイル基準
         PERFORM  JB2-READ-SEC
         IF  FG-JISSSB2-END = 9
             MOVE  "END"    TO  FG-ZEN-JISSEKI-RD-END
         END-IF

       WHEN  IX-FIL = 2  *> 実績週集計退避２ファイル基準
         PERFORM  JB3-READ-SEC
         IF  FG-JISSSB3-END = 9
             MOVE  "END"    TO  FG-ZEN-JISSEKI-RD-END
         END-IF

       WHEN  OTHER
         MOVE  "END"        TO  FG-ZEN-JISSEKI-RD-END

     END-EVALUATE.

 ZEN-JISSEKI-RD-EXIT.
     EXIT.
*=============================================================
*  帳票出力Ｃ
*=============================================================
 PRINTC-SEC           SECTION.

     INITIALIZE  WK-S-AREA.
     INITIALIZE  WK-G-AREA.

     IF  INF-F02  NOT = BRK-F02  *> 年週ブレ－ク
         MOVE 66            TO  LINE-CNT
     END-IF.
*  集計の先頭レコードを退避。

     EVALUATE  TRUE
       WHEN  IX-FIL = ZERO *> 実績週集計ファイル
         MOVE  JSS-REC      TO  JSSW-REC

       WHEN  IX-FIL = 1  *> 実績週集計退避１ファイル
         MOVE  JB1-REC      TO  JB1W-REC

       WHEN  IX-FIL = 2  *> 実績週集計退避２ファイル
         MOVE  JB2-REC      TO  JB2W-REC

       WHEN  IX-FIL = 3  *> 実績週集計退避３ファイル
         MOVE  JB3-REC      TO  JB3W-REC

     END-EVALUATE.

* 該当年集計
     PERFORM  SUM-SEC.

* 前年集計
     PERFORM  ZEN-SUM-SEC.

* 印刷
     PERFORM  PRINTD-SEC.

 PRINTC-EXIT.
     EXIT.
*=============================================================
*  入力データ集計処理
*=============================================================
 SUM-SEC           SECTION.
     MOVE  INF-KEY          TO  BRK-KEY.

     PERFORM  UNTIL FG-JISSEKI-RD-END = "END"
                 OR INF-KEY       NOT = BRK-KEY
                                  *> 年、週番、取引先ＣＤ
       PERFORM  SUMB-SEC
       PERFORM  JISSEKI-RD-SEC

     END-PERFORM.

 SUM-EXIT.
     EXIT.
*=============================================================
*  入力データ集計処理
*=============================================================
 SUMB-SEC          SECTION.
     MOVE "TOU"             TO  FG-TOUZEN.
     PERFORM  ICHI-BNR20-SEC.
     MOVE  IX-BNR20      TO  IX.

     EVALUATE  TRUE
       WHEN  IX-FIL = ZERO *> 実績週集計ファイル
         ADD  JSS-F06    TO  WK-S-SURYO  (IX 1)
         ADD  JSS-F07    TO  WK-S-KINGAK (IX 1)
         ADD  JSS-F08    TO  WK-S-HEPSU  (IX 1)
         ADD  JSS-F09    TO  WK-S-HEPGAK (IX 1)

         ADD  JSS-F06    TO  WK-G-SURYO  (1)
         ADD  JSS-F07    TO  WK-G-KINGAK (1)
         ADD  JSS-F08    TO  WK-G-HEPSU  (1)
         ADD  JSS-F09    TO  WK-G-HEPGAK (1)

         ADD  JSS-F06    TO  WK-T-SURYO  (1)
         ADD  JSS-F07    TO  WK-T-KINGAK (1)
         ADD  JSS-F08    TO  WK-T-HEPSU  (1)
         ADD  JSS-F09    TO  WK-T-HEPGAK (1)

       WHEN  IX-FIL = 1  *> 実績週集計退避１ファイル
         ADD  JB1-F06    TO  WK-S-SURYO  (IX 1)
         ADD  JB1-F07    TO  WK-S-KINGAK (IX 1)
         ADD  JB1-F08    TO  WK-S-HEPSU  (IX 1)
         ADD  JB1-F09    TO  WK-S-HEPGAK (IX 1)

         ADD  JB1-F06    TO  WK-G-SURYO  (1)
         ADD  JB1-F07    TO  WK-G-KINGAK (1)
         ADD  JB1-F08    TO  WK-G-HEPSU  (1)
         ADD  JB1-F09    TO  WK-G-HEPGAK (1)

         ADD  JB1-F06    TO  WK-T-SURYO  (1)
         ADD  JB1-F07    TO  WK-T-KINGAK (1)
         ADD  JB1-F08    TO  WK-T-HEPSU  (1)
         ADD  JB1-F09    TO  WK-T-HEPGAK (1)

       WHEN  IX-FIL = 2  *> 実績週集計退避２ファイル
         ADD  JB2-F06    TO  WK-S-SURYO  (IX 1)
         ADD  JB2-F07    TO  WK-S-KINGAK (IX 1)
         ADD  JB2-F08    TO  WK-S-HEPSU  (IX 1)
         ADD  JB2-F09    TO  WK-S-HEPGAK (IX 1)

         ADD  JB2-F06    TO  WK-G-SURYO  (1)
         ADD  JB2-F07    TO  WK-G-KINGAK (1)
         ADD  JB2-F08    TO  WK-G-HEPSU  (1)
         ADD  JB2-F09    TO  WK-G-HEPGAK (1)

         ADD  JB2-F06    TO  WK-T-SURYO  (1)
         ADD  JB2-F07    TO  WK-T-KINGAK (1)
         ADD  JB2-F08    TO  WK-T-HEPSU  (1)
         ADD  JB2-F09    TO  WK-T-HEPGAK (1)

       WHEN  IX-FIL = 3  *> 実績週集計退避３ファイル
         ADD  JB3-F06    TO  WK-S-SURYO  (IX 1)
         ADD  JB3-F07    TO  WK-S-KINGAK (IX 1)
         ADD  JB3-F08    TO  WK-S-HEPSU  (IX 1)
         ADD  JB3-F09    TO  WK-S-HEPGAK (IX 1)

         ADD  JB3-F06    TO  WK-G-SURYO  (1)
         ADD  JB3-F07    TO  WK-G-KINGAK (1)
         ADD  JB3-F08    TO  WK-G-HEPSU  (1)
         ADD  JB3-F09    TO  WK-G-HEPGAK (1)

         ADD  JB3-F06    TO  WK-T-SURYO  (1)
         ADD  JB3-F07    TO  WK-T-KINGAK (1)
         ADD  JB3-F08    TO  WK-T-HEPSU  (1)
         ADD  JB3-F09    TO  WK-T-HEPGAK (1)

     END-EVALUATE.

 SUMB-EXIT.
     EXIT.
*=============================================================
*  サカタ２０分類位置判定
*=============================================================
 ICHI-BNR20-SEC             SECTION.
     MOVE  ZERO             TO  IX-BNR20.
     MOVE  SPACE            TO  WK-BNR20.

     IF  FG-TOUZEN = "TOU" *> 該当年

         EVALUATE  TRUE
           WHEN  IX-FIL = ZERO *> 実績週集計ファイル
             MOVE  JSS-F13  TO  WK-BNR20
           WHEN  IX-FIL = 1  *> 実績週集計退避１ファイル
             MOVE  JB1-F13  TO  WK-BNR20
           WHEN  IX-FIL = 2  *> 実績週集計退避２ファイル
             MOVE  JB2-F13  TO  WK-BNR20
           WHEN  IX-FIL = 3  *> 実績週集計退避３ファイル
             MOVE  JB3-F13  TO  WK-BNR20
         END-EVALUATE

     ELSE                  *>前年
         EVALUATE  TRUE
           WHEN  IX-FIL = ZERO  *> 実績週集計ファイル基準
             MOVE  JB1-F13  TO  WK-BNR20
           WHEN  IX-FIL = 1  *> 実績週集計退避２ファイル基準
             MOVE  JB2-F13  TO  WK-BNR20
           WHEN  IX-FIL = 2  *> 実績週集計退避３ファイル基準
             MOVE  JB3-F13  TO  WK-BNR20
         END-EVALUATE

     END-IF.

     PERFORM   VARYING IX-ICHI-BNR20  FROM 1 BY 1
               UNTIL   IX-ICHI-BNR20 > 20
       IF  TB-BNR20 (IX-ICHI-BNR20) = WK-BNR20
           MOVE  IX-ICHI-BNR20   TO  IX-BNR20
           MOVE  20              TO  IX-ICHI-BNR20
       END-IF
     END-PERFORM.

     IF IX-BNR20 = ZERO
        *> 範囲外があった場合その他（CD="20"）とする。
        MOVE  19            TO  IX-BNR20
     END-IF.
 ICHI-BNR20-EXIT.
     EXIT.
*=============================================================
*  前年入力データ集計処理
*=============================================================
 ZEN-SUM-SEC           SECTION.

     IF  ZEN-INF-KEY2 < BRK-KEY2 *> 週番、取引先ＣＤ
         PERFORM  UNTIL  FG-ZEN-JISSEKI-RD-END = "END"
                      OR ZEN-INF-KEY2 >= BRK-KEY2
           PERFORM  ZEN-JISSEKI-RD-SEC

         END-PERFORM
     END-IF.

     IF  ZEN-INF-KEY2 = BRK-KEY2 *> 週番、取引先ＣＤ
         PERFORM  UNTIL  FG-ZEN-JISSEKI-RD-END = "END"
                      OR ZEN-INF-KEY2 > BRK-KEY2
           PERFORM  ZEN-SUMB-SEC
           PERFORM  ZEN-JISSEKI-RD-SEC

         END-PERFORM

     END-IF.

 ZEN-SUM-EXIT.
     EXIT.
*=============================================================
*  前年入力データ集計Ｂ処理
*=============================================================
 ZEN-SUMB-SEC           SECTION.
     MOVE "ZEN"             TO  FG-TOUZEN.
     PERFORM  ICHI-BNR20-SEC.
     MOVE  IX-BNR20         TO  IX.

     EVALUATE  TRUE
       WHEN  IX-FIL = ZERO  *> 実績週集計ファイル基準
         ADD  JB1-F06    TO  WK-S-SURYO  (IX 2)
         ADD  JB1-F07    TO  WK-S-KINGAK (IX 2)
         ADD  JB1-F08    TO  WK-S-HEPSU  (IX 2)
         ADD  JB1-F09    TO  WK-S-HEPGAK (IX 2)

         ADD  JB1-F06    TO  WK-G-SURYO  (2)
         ADD  JB1-F07    TO  WK-G-KINGAK (2)
         ADD  JB1-F08    TO  WK-G-HEPSU  (2)
         ADD  JB1-F09    TO  WK-G-HEPGAK (2)

         ADD  JB1-F06    TO  WK-T-SURYO  (2)
         ADD  JB1-F07    TO  WK-T-KINGAK (2)
         ADD  JB1-F08    TO  WK-T-HEPSU  (2)
         ADD  JB1-F09    TO  WK-T-HEPGAK (2)

       WHEN  IX-FIL = 1  *> 実績週集計退避１ファイル基準
         ADD  JB2-F06    TO  WK-S-SURYO  (IX 2)
         ADD  JB2-F07    TO  WK-S-KINGAK (IX 2)
         ADD  JB2-F08    TO  WK-S-HEPSU  (IX 2)
         ADD  JB2-F09    TO  WK-S-HEPGAK (IX 2)

         ADD  JB2-F06    TO  WK-G-SURYO  (2)
         ADD  JB2-F07    TO  WK-G-KINGAK (2)
         ADD  JB2-F08    TO  WK-G-HEPSU  (2)
         ADD  JB2-F09    TO  WK-G-HEPGAK (2)

         ADD  JB2-F06    TO  WK-T-SURYO  (2)
         ADD  JB2-F07    TO  WK-T-KINGAK (2)
         ADD  JB2-F08    TO  WK-T-HEPSU  (2)
         ADD  JB2-F09    TO  WK-T-HEPGAK (2)

       WHEN  IX-FIL = 2  *> 実績週集計退避２ファイル基準
         ADD  JB3-F06    TO  WK-S-SURYO  (IX 2)
         ADD  JB3-F07    TO  WK-S-KINGAK (IX 2)
         ADD  JB3-F08    TO  WK-S-HEPSU  (IX 2)
         ADD  JB3-F09    TO  WK-S-HEPGAK (IX 2)

         ADD  JB3-F06    TO  WK-G-SURYO  (2)
         ADD  JB3-F07    TO  WK-G-KINGAK (2)
         ADD  JB3-F08    TO  WK-G-HEPSU  (2)
         ADD  JB3-F09    TO  WK-G-HEPGAK (2)

         ADD  JB3-F06    TO  WK-T-SURYO  (2)
         ADD  JB3-F07    TO  WK-T-KINGAK (2)
         ADD  JB3-F08    TO  WK-T-HEPSU  (2)
         ADD  JB3-F09    TO  WK-T-HEPGAK (2)

     END-EVALUATE.

 ZEN-SUMB-EXIT.
     EXIT.
*=============================================================
*  帳票出力Ｄ
*=============================================================
 PRINTD-SEC           SECTION.
     PERFORM  BODY-PRINT-SEC.
     PERFORM  GOKEI-PRINT-SEC.
 PRINTD-EXIT.
     EXIT.
*=============================================================
*                明細印刷処理
*=============================================================
 BODY-PRINT-SEC             SECTION.
     IF  DSP-BNR20 = "1" *> サカタ２０分類実績分のみ
         IF      WK-G-SURYO  (1) = ZERO
             AND WK-G-KINGAK (1) = ZERO
             AND WK-G-HEPSU  (1) = ZERO
             AND WK-G-HEPGAK (1) = ZERO
             AND WK-G-SURYO  (2) = ZERO
             AND WK-G-KINGAK (2) = ZERO
             AND WK-G-HEPSU  (2) = ZERO
             AND WK-G-HEPGAK (2) = ZERO
             GO TO  BODY-PRINT-EXIT
         END-IF
     END-IF.


     IF  DSP-BNR20 = "1" *> サカタ２０分類実績分のみ
         MOVE  ZERO         TO  IX-MSGYO-MAX
         MOVE  LINE-CNT     TO  WK-LINE-CNT
         ADD  1  TO WK-LINE-CNT  *> 取引先ＣＤ
         PERFORM  VARYING IX  FROM 1 BY 1 *> 明細
                  UNTIL   IX > 20
           IF      WK-S-SURYO  (IX 1) = ZERO
               AND WK-S-KINGAK (IX 1) = ZERO
               AND WK-S-HEPSU  (IX 1) = ZERO
               AND WK-S-HEPGAK (IX 1) = ZERO
               AND WK-S-SURYO  (IX 2) = ZERO
               AND WK-S-KINGAK (IX 2) = ZERO
               AND WK-S-HEPSU  (IX 2) = ZERO
               AND WK-S-HEPGAK (IX 2) = ZERO
               CONTINUE
            ELSE
               MOVE  IX     TO  IX-MSGYO-MAX
               ADD  2  TO WK-LINE-CNT
           END-IF

         END-PERFORM

         ADD  4  TO WK-LINE-CNT *> 仕入先計

         PERFORM  HEAD-PRINT-SEC
     ELSE                *> サカタ２０分類全て
         MOVE  20           TO  IX-MSGYO-MAX
         COMPUTE  WK-LINE-CNT = LINE-CNT + 46
         PERFORM  HEAD-PRINT-SEC
     END-IF.

     PERFORM  BODY-PRINTB-SEC. *> 仕入先行

     PERFORM  VARYING IX  FROM 1 BY 1
              UNTIL   IX > 20
       PERFORM  BODY-PRINTB2-SEC *> 明細行

     END-PERFORM.

 BODY-PRINT-EXIT.
     EXIT.
*=============================================================
*                ＨＥＡＤ部　印刷処理
*=============================================================
 HEAD-PRINT-SEC      SECTION.
     IF  WK-LINE-CNT >= MAX-LINE
         PERFORM  HEAD-PRINTB-SEC
     END-IF.

 HEAD-PRINT-EXIT.
     EXIT.
*=============================================================
*                ＨＥＡＤ部　印刷Ｂ処理
*=============================================================
 HEAD-PRINTB-SEC      SECTION.
     ADD  1   TO  PAGE-CNT.
     MOVE  PAGE-CNT         TO  H1-PAGE.

     MOVE  SYS-DATE(1:4)    TO  H1-YY.
     MOVE  SYS-DATE(5:2)    TO  H1-MM.
     MOVE  SYS-DATE(7:2)    TO  H1-DD.

     EVALUATE  TRUE
       WHEN  IX-FIL = ZERO  *> 実績週集計ファイル
         MOVE  JSSW-F021    TO  H2-NEN
         MOVE  JSSW-F022    TO  H2-SYU

       WHEN  IX-FIL = 1  *> 実績週集計退避１ファイル
         MOVE  JB1W-F021    TO  H2-NEN
         MOVE  JB1W-F022    TO  H2-SYU

       WHEN  IX-FIL = 2  *> 実績週集計退避２ファイル
         MOVE  JB2W-F021    TO  H2-NEN
         MOVE  JB2W-F022    TO  H2-SYU

       WHEN  IX-FIL = 3  *> 実績週集計退避３ファイル
         MOVE  JB3W-F021    TO  H2-NEN
         MOVE  JB3W-F022    TO  H2-SYU

     END-EVALUATE.

     IF  PAGE-CNT NOT = 1
         MOVE  SPACE        TO  P-REC
         WRITE  P-REC  AFTER PAGE
     END-IF.

     WRITE  P-REC  FROM MIDASI1    AFTER 2.
     WRITE  P-REC  FROM MIDASI2    AFTER 1.
     WRITE  P-REC  FROM MIDASI9    AFTER 1.
     WRITE  P-REC  FROM MIDASI3    AFTER 1.
     WRITE  P-REC  FROM MIDASI3-2  AFTER 1.
     WRITE  P-REC  FROM MIDASI9    AFTER 1.

     MOVE  8                TO  LINE-CNT.
 HEAD-PRINTB-EXIT.
     EXIT.
*=============================================================
*                明細印刷Ｂ処理（仕入先）
*=============================================================
 BODY-PRINTB-SEC             SECTION.

     EVALUATE  TRUE
       WHEN  IX-FIL = ZERO  *> 実績週集計ファイル
         MOVE  JSSW-F03     TO  BUT-F01

       WHEN  IX-FIL = 1  *> 実績週集計退避１ファイル
         MOVE  JB1W-F03     TO  BUT-F01

       WHEN  IX-FIL = 2  *> 実績週集計退避２ファイル
         MOVE  JB2W-F03     TO  BUT-F01

       WHEN  IX-FIL = 3  *> 実績週集計退避３ファイル
         MOVE  JB3W-F03     TO  BUT-F01

     END-EVALUATE.

     READ  BUTOKMF
       INVALID
         MOVE 1             TO  FG-BUTOKMF-INV
       NOT INVALID
         MOVE ZERO          TO  FG-BUTOKMF-INV
     END-READ.

     MOVE  SPACE            TO  MEISAI1.

     EVALUATE  TRUE
       WHEN  IX-FIL = ZERO  *> 実績週集計ファイル
         MOVE  JSSW-F03     TO  PRT-TOKCD

       WHEN  IX-FIL = 1  *> 実績週集計退避１ファイル
         MOVE  JB1W-F03     TO  PRT-TOKCD

       WHEN  IX-FIL = 2  *> 実績週集計退避２ファイル
         MOVE  JB2W-F03     TO  PRT-TOKCD

       WHEN  IX-FIL = 3  *> 実績週集計退避３ファイル
         MOVE  JB3W-F03     TO  PRT-TOKCD

     END-EVALUATE.

     IF FG-BUTOKMF-INV = ZERO
        MOVE  BUT-F02       TO  PRT-TOKNM
     END-IF.

     WRITE  P-REC  FROM MEISAI1  AFTER 1.
     ADD  1   TO  LINE-CNT.

 BODY-PRINTB-EXIT.
     EXIT.
*=============================================================
*                明細印刷Ｂ２処理（明細）
*=============================================================
 BODY-PRINTB2-SEC             SECTION.
     IF  DSP-BNR20 = "1" *> サカタ２０分類実績分のみ
         IF      WK-S-SURYO  (IX 1) = ZERO
             AND WK-S-KINGAK (IX 1) = ZERO
             AND WK-S-HEPSU  (IX 1) = ZERO
             AND WK-S-HEPGAK (IX 1) = ZERO
             AND WK-S-SURYO  (IX 2) = ZERO
             AND WK-S-KINGAK (IX 2) = ZERO
             AND WK-S-HEPSU  (IX 2) = ZERO
             AND WK-S-HEPGAK (IX 2) = ZERO
             GO TO  BODY-PRINTB2-EXIT
         END-IF
     END-IF.

* 該当年サカタ２０分類明細
     MOVE  SPACE            TO  MEISAI2.

     MOVE  TB-BNR20    (IX)      TO  PRT2-BNR20.
     MOVE  TB-BNR20NM  (IX)      TO  PRT2-BNR20NM.
     MOVE  WK-S-SURYO  (IX 1)    TO  PRT2-SURYO.
     MOVE  WK-S-KINGAK (IX 1)    TO  PRT2-KINGAK.
     MOVE  WK-S-HEPSU  (IX 1)    TO  PRT2-HEPSU.
     MOVE  WK-S-HEPGAK (IX 1)    TO  PRT2-HEPGAK.
     COMPUTE  PRT2-SA-SU = WK-S-SURYO (IX 1) - WK-S-HEPSU (IX 1).
     COMPUTE  PRT2-SA-GAK =
         WK-S-KINGAK (IX 1) - WK-S-HEPGAK (IX 1).
     WRITE  P-REC  FROM MEISAI2  AFTER 1.

*前年サカタ２０分類明細
     MOVE  SPACE            TO  MEISAI3.

     IF  IX NOT = IX-MSGYO-MAX
         *> 最終明細行以外は区切り表示する。*> 2012/01/16 廃止
****     MOVE  ALL "_"      TO  PRT3-G
         MOVE  SPACE        TO  PRT3-G
     END-IF.

     MOVE  WK-S-SURYO  (IX 2)    TO  PRT3-SURYO.
     MOVE  WK-S-KINGAK (IX 2)    TO  PRT3-KINGAK.
     MOVE  WK-S-HEPSU  (IX 2)    TO  PRT3-HEPSU.
     MOVE  WK-S-HEPGAK (IX 2)    TO  PRT3-HEPGAK.
     COMPUTE  PRT3-SA-SU = WK-S-SURYO (IX 2) - WK-S-HEPSU (IX 2).
     COMPUTE  PRT3-SA-GAK =
         WK-S-KINGAK (IX 2) - WK-S-HEPGAK (IX 2).
     WRITE  P-REC  FROM MEISAI3  AFTER 1.

     ADD  2   TO  LINE-CNT.

 BODY-PRINTB2-EXIT.
     EXIT.
*=============================================================
*                合計印刷処理
*=============================================================
 GOKEI-PRINT-SEC            SECTION.
     COMPUTE  WK-LINE-CNT = LINE-CNT + 3.
     PERFORM  HEAD-PRINT-SEC.

     PERFORM  GOKEI-PRINTB-SEC.  *> 仕入先計
     PERFORM  GOKEI-PRINTB2-SEC. *> 前年同週仕入先計

 GOKEI-PRINT-EXIT.
     EXIT.
*=============================================================
*                合計印刷Ｂ処理
*=============================================================
 GOKEI-PRINTB-SEC           SECTION.
     MOVE  SPACE            TO  MEISAI2.

     MOVE  NC"　　　　　　　仕　入　先　計　"
                            TO  PRT2-BNR20NM.
     MOVE  WK-G-SURYO (1)   TO  PRT2-SURYO.
     MOVE  WK-G-HEPSU (1)   TO  PRT2-HEPSU.
     COMPUTE  PRT2-SA-SU = WK-G-SURYO (1) - WK-G-HEPSU (1).

     MOVE  WK-G-KINGAK (1)  TO  PRT2-KINGAK.
     MOVE  WK-G-HEPGAK (1)  TO  PRT2-HEPGAK.
     COMPUTE  PRT2-SA-GAK =
         WK-G-KINGAK (1) - WK-G-HEPGAK (1).
*印刷
     WRITE  P-REC  FROM MEISAI9  AFTER 1.
     WRITE  P-REC  FROM MEISAI2  AFTER 1.
     ADD  2   TO  LINE-CNT.

 GOKEI-PRINTB-EXIT.
     EXIT.
*=============================================================
*                合計印刷Ｂ２処理
*=============================================================
 GOKEI-PRINTB2-SEC           SECTION.
     MOVE  SPACE            TO  MEISAI2.

     MOVE  NC"　　　　　　　前年同週仕入先計"
                            TO  PRT2-BNR20NM.
     MOVE  WK-G-SURYO (2)   TO  PRT2-SURYO.
     MOVE  WK-G-HEPSU (2)   TO  PRT2-HEPSU.
     COMPUTE  PRT2-SA-SU = WK-G-SURYO (2) - WK-G-HEPSU (2).

     MOVE  WK-G-KINGAK (2)  TO  PRT2-KINGAK.
     MOVE  WK-G-HEPGAK (2)  TO  PRT2-HEPGAK.
     COMPUTE  PRT2-SA-GAK =
         WK-G-KINGAK (2) - WK-G-HEPGAK (2).
*印刷
     WRITE  P-REC  FROM MEISAI2  AFTER 1.
     WRITE  P-REC  FROM MEISAI9  AFTER 1.
     ADD  2   TO  LINE-CNT.

 GOKEI-PRINTB2-EXIT.
     EXIT.
*=============================================================
*    総合計処理
*=============================================================
 SOKEI-PRINT-SEC            SECTION.
     COMPUTE  WK-LINE-CNT = LINE-CNT + 3.
     PERFORM  HEAD-PRINT-SEC.

     PERFORM  SOKEI-PRINTB-SEC.  *> 総合計
     PERFORM  SOKEI-PRINTB2-SEC. *> 前年同週総合計

 SOKEI-PRINT-EXIT.
     EXIT.
*=============================================================
*    総合計印刷Ｂ処理
*=============================================================
 SOKEI-PRINTB-SEC           SECTION.
     MOVE  SPACE            TO  MEISAI2.

     MOVE  NC"　　　　　　　総　合　計　　　"
                            TO  PRT2-BNR20NM.
     MOVE  WK-T-SURYO (1)   TO  PRT2-SURYO.
     MOVE  WK-T-HEPSU (1)   TO  PRT2-HEPSU.
     COMPUTE  PRT2-SA-SU = WK-T-SURYO (1) - WK-T-HEPSU (1).

     MOVE  WK-T-KINGAK (1)  TO  PRT2-KINGAK.
     MOVE  WK-T-HEPGAK (1)  TO  PRT2-HEPGAK.
     COMPUTE  PRT2-SA-GAK =
         WK-T-KINGAK (1) - WK-T-HEPGAK (1).
*印刷
     WRITE  P-REC  FROM MEISAI2  AFTER 1.
     ADD  1   TO  LINE-CNT.

 SOKEI-PRINTB-EXIT.
     EXIT.
*=============================================================
*    総合計印刷Ｂ２処理
*=============================================================
 SOKEI-PRINTB2-SEC           SECTION.

     MOVE  SPACE            TO  MEISAI2.

     MOVE  NC"　　　　　　　前年同週総合計　"
                            TO  PRT2-BNR20NM.
     MOVE  WK-T-SURYO (2)   TO  PRT2-SURYO.
     MOVE  WK-T-HEPSU (2)   TO  PRT2-HEPSU.
     COMPUTE  PRT2-SA-SU = WK-T-SURYO (2) - WK-T-HEPSU (2).

     MOVE  WK-T-KINGAK (2)  TO  PRT2-KINGAK.
     MOVE  WK-T-HEPGAK (2)  TO  PRT2-HEPGAK.
     COMPUTE  PRT2-SA-GAK =
         WK-T-KINGAK (2) - WK-T-HEPGAK (2).
*印刷
     WRITE  P-REC  FROM MEISAI2  AFTER 1.
     WRITE  P-REC  FROM MEISAI9  AFTER 1.
     ADD  2   TO  LINE-CNT.

 SOKEI-PRINTB2-EXIT.
     EXIT.

```
