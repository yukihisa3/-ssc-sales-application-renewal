# SPD0115L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SPD0115L.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　サカタのタネ（株）殿　　　　　　　*
*    業務名　　　　　　　：　販売管理システム　　　　　　　　　*
*    モジュール名　　　　：　取込確認リスト　　　　　　　　　　*
*    作成日／作成者　　　：　09/10/19  OONO                    *
*    　　　　　　　　　　：　画面日付，時刻表示　　　　　　　　*
*                        ：　日付変換サブルーチン使用　　　　　*
*    再利用ＰＧ　　　　　：  SSKT040.SKTSLIB                   *
*    処理概要　　　　　　：　量販店ＪＮＬ（ＰＣ連携用）をメモ_*
*                        ：　で読み、エラー区分に値が在る場合は*
*    　　　　　　　　　　：　リストに出力する                  *
*    更新日／更新者　　　：　2016/08/17 NAV TAKAHASHI          *
*    処理概要　　　　　　：　担当者ＣＤをログイン担当者に変更す*
*                        ：　る。ファイル上数値項目の為、文字を*
*    　　　　　　　　　　：　含む担当者ＣＤが化けて部分の対策  *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SPD0115L.
 AUTHOR.                OONO.
 DATE-WRITTEN.          09/10/19.
 DATE-COMPILED.
 SECURITY.              NONE.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM-K150.
 OBJECT-COMPUTER.       FACOM-K150.
 SPECIAL-NAMES.
         YB        IS   PITCH-1-5
         YB-21     IS   BAIKAKU-1-5
         YA-21     IS   BAIKAKU
         YA        IS   HYOUJUN
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 量販店ＪＮＬ >>--*
     SELECT   PCRYOJF   ASSIGN         DA-01-VI-PCRYOJL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  PCR-F011  PCR-F012  PCR-F02
                        STATUS         PCRYOJF-ST.
*----<< 取引先マスタ >>--*
     SELECT   HTOKMS    ASSIGN         DA-01-VI-TOKMS2
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  TOK-F01
                        STATUS         HTOKMS-ST.
*----<< 商品変換テーブル >>--*
     SELECT   HSHOTBL   ASSIGN         DA-01-VI-SHOTBL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  SHO-F01   SHO-F02
                        STATUS         HSHOTBL-ST.
*
*----<< 担当者マスタ >>--*
     SELECT   HTANMS    ASSIGN         DA-01-VI-TANMS1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  TAN-F01   TAN-F02
                        STATUS         HTANMS-ST.
*
*----<< プリンタ >>-*
     SELECT   PRTF      ASSIGN         LP-04.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 量販店ＪＮＬ >>--*
 FD  PCRYOJF            LABEL     RECORD   IS   STANDARD.
     COPY     PCRYOJF   OF        XFDLIB
              JOINING   PCR       PREFIX.
*----<< 取引先マスタ >>--*
 FD  HTOKMS             LABEL RECORD   IS   STANDARD.
     COPY     HTOKMS    OF        XFDLIB
              JOINING   TOK       PREFIX.
*----<< 商品変換テーブル >>--*
 FD  HSHOTBL            LABEL RECORD   IS   STANDARD.
     COPY     HSHOTBL   OF        XFDLIB
              JOINING   SHO       PREFIX.
*
*----<< 担当者マスタ >>--*
 FD  HTANMS             LABEL RECORD   IS   STANDARD.
     COPY     HTANMS    OF        XFDLIB
              JOINING   TAN       PREFIX.
*
*----<< プリンタ >>-*
 FD  PRTF               LABEL RECORD   IS   OMITTED.
 01  PRT-REC            PIC  X(200).
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  SKIP-FLG       PIC  9(01).
     03  SEN-FLG        PIC  9(01)     VALUE ZERO.
     03  WRT-FLG        PIC  9(01)     VALUE ZERO.
     03  MS4-FLG        PIC  9(01)     VALUE ZERO.
     03  M2B-FLG        PIC  9(01)     VALUE ZERO.
 01  COUNTERS.
     03  LINE-CNT       PIC  9(03).
     03  PAGE-CNT       PIC  9(03).
 01  IDX.
     03  I              PIC  9(03).
     03  IX             PIC  9(03).
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  PCRYOJF-ST        PIC  X(02).
 01  HTOKMS-ST         PIC  X(02).
 01  HSHOTBL-ST        PIC  X(02).
 01  HTANMS-ST         PIC  X(02).
*
*----<< ﾌﾟﾘﾝﾄ ﾜｰｸ >>--*
 01  WK-032.
     03  WK-0321        PIC  X(05).
     03  WK-0322        PIC  X(02).
     03  WK-0323        PIC  X(01).
*
*----<< ｺﾞｳｹｲ ﾜｰｸ >>--*
 01  GOKEI-AREA.
     03  KEI-SUU        PIC  S9(11)    PACKED-DECIMAL.
*
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
 01  SYS-DATE.
     03  WK-YS          PIC  9(02)  VALUE ZERO.
     03  WK-DATE.
         05  WK-Y       PIC  9(02)  VALUE ZERO.
         05  WK-M       PIC  9(02)  VALUE ZERO.
         05  WK-D       PIC  9(02)  VALUE ZERO.
 01  SYS-TIME           PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME.
     03  SYS-HH         PIC  9(02).
     03  SYS-MN         PIC  9(02).
     03  SYS-SS         PIC  9(02).
     03  SYS-MS         PIC  9(02).
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
*
*----<< ﾊﾝｲ ﾜｰｸ >>--*
 01  ST-MEMO-X.
     03  ST-MEMO        PIC  9(04).
 01  ED-MEMO-X.
     03  ED-MEMO        PIC  9(04).
*
*----<< BREAK KEY >>--*
 01  BREAK-KEY.
     03  NEW.
         05  NEW-01.
             07  NEW-MEMO-1                 PIC  9(04).
             07  NEW-MEMO-2                 PIC  9(04).
     03  OLD.
         05  OLD-01.
             07  OLD-MEMO-1                 PIC  9(04).
             07  OLD-MEMO-2                 PIC  9(04).
     03  NEW02.
         05  MEMO02                         PIC  9(04).
*----<< ﾃﾞｨｽﾌﾟﾚｲ ｺﾝﾄﾛｰﾙ ｴﾘｱ >>-*
 01  GR-NO              PIC  9(02)    VALUE  ZERO.
 01  WK-GRP             PIC  X(08).
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*
*--<< ﾌﾟﾘﾝﾄ AREA >>-*
 01  HEAD01.
     03  FILLER.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  HD-001     PIC  X(08)     VALUE  ALL "X".
         05  FILLER     PIC  X(25)     VALUE  SPACE.
     03  FILLER         CHARACTER TYPE BAIKAKU.
         05  FILLER     PIC  N(16)     VALUE
                        NC"＜取込確認リスト兼エラーリスト＞".
     03  FILLER         CHARACTER TYPE HYOUJUN.
         05  FILLER     PIC  X(20)     VALUE  SPACE.
         05  HD-011     PIC  9999.
         05  FILLER     PIC  N(01)     VALUE  NC"年".
         05  HD-012     PIC  Z9.
         05  FILLER     PIC  N(01)     VALUE  NC"月".
         05  HD-013     PIC  Z9.
         05  FILLER     PIC  N(01)     VALUE  NC"日".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  HD-014     PIC  ZZ9.
         05  FILLER     PIC  N(01)     VALUE  NC"頁".
 01  WAKU1.
     03  FILLER.
         05  FILLER     PIC  X(116)    VALUE  SPACE.
         05  FILLER     PIC  X(19)     VALUE
                              "+-----+-----+-----+".
 01  WAKU2.
     03  FILLER.
         05  FILLER     PIC  X(116)    VALUE  SPACE.
         05  FILLER     PIC  X(19)     VALUE
                              "!     !     !     !".
 01  WAKU3.
     03  FILLER.
         05  FILLER     PIC  X(116)    VALUE  SPACE.
         05  FILLER     PIC  X(19)     VALUE
                              "!     !     !     !".
 01  WAKU4.
     03  FILLER.
         05  FILLER     PIC  X(116)    VALUE  SPACE.
         05  FILLER     PIC  X(19)     VALUE
                              "!     !     !     !".
 01  WAKU5.
     03  FILLER.
         05  FILLER     PIC  X(116)    VALUE  SPACE.
         05  FILLER     PIC  X(19)     VALUE
                              "+-----+-----+-----+".
 01  HEAD002.
     03  FILLER         CHARACTER TYPE HYOUJUN.
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  X(03)     VALUE  "(1:".
         05  FILLER     PIC  N(08)     VALUE
                             NC"取引先ＣＤ未登録".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  X(02)     VALUE  "2:".
         05  FILLER     PIC  N(07)     VALUE
                             NC"店舗ＣＤ未登録".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  X(02)     VALUE  "3:".
         05  FILLER     PIC  N(06)     VALUE
                             NC"納品日範囲外".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  X(02)     VALUE  "4:".
         05  FILLER     PIC  X(04)     VALUE  "ACOS".
         05  FILLER     PIC  N(02)     VALUE
                             NC"締日".
         05  FILLER     PIC  X(03)     VALUE  "ｴﾗｰ".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  X(02)     VALUE  "5:".
         05  FILLER     PIC  N(04)     VALUE
                             NC"商品変換".
         05  FILLER     PIC  X(03)     VALUE  "TBL".
         05  FILLER     PIC  N(03)     VALUE
                             NC"未登録".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  X(02)     VALUE  "6:".
         05  FILLER     PIC  N(04)     VALUE
                             NC"商品名称".
         05  FILLER     PIC  X(01)     VALUE  "M".
         05  FILLER     PIC  N(03)     VALUE
                             NC"未登録".
         05  FILLER     PIC  X(01)     VALUE  ")".
 01  HEAD02.
     03  FILLER.
       05  FILLER         CHARACTER TYPE HYOUJUN.
         07  FILLER     PIC  X(01)     VALUE  SPACE.
         07  FILLER     PIC  N(03)     VALUE  NC"取引先".
         07  FILLER     PIC  X(24)     VALUE  SPACE.
         07  FILLER     PIC  X(04)     VALUE  "ﾒﾓNO".
         07  FILLER     PIC  X(06)     VALUE  SPACE.
         07  FILLER     PIC  N(02)     VALUE  NC"出場".
         07  FILLER     PIC  X(01)     VALUE  SPACE.
         07  FILLER     PIC  N(02)     VALUE  NC"伝場".
         07  FILLER     PIC  X(01)     VALUE  SPACE.
         07  FILLER     PIC  N(03)     VALUE  NC"注文_".
         07  FILLER     PIC  X(04)     VALUE  SPACE.
         07  FILLER     PIC  N(03)     VALUE  NC"注文日".
         07  FILLER     PIC  X(03)     VALUE  SPACE.
         07  FILLER     PIC  N(03)     VALUE  NC"納品日".
         07  FILLER     PIC  X(02)     VALUE  SPACE.
         07  FILLER     PIC  N(02)     VALUE  NC"分類".
         07  FILLER     PIC  X(01)     VALUE  SPACE.
       05  FILLER         CHARACTER TYPE PITCH-1-5.
         07  FILLER     PIC  N(02)     VALUE  NC"商区".
         07  FILLER     PIC  X(01)     VALUE  SPACE.
         07  FILLER     PIC  N(02)     VALUE  NC"伝票".
         07  FILLER     PIC  X(01)     VALUE  SPACE.
         07  FILLER     PIC  N(02)     VALUE  NC"伝発".
*
         07  FILLER     PIC  X(01)     VALUE  SPACE.
         07  FILLER     PIC  N(02)     VALUE  NC"担当".
*
       05  FILLER.
         07  FILLER     PIC  X(07)     VALUE  SPACE.
         07  FILLER     PIC  X(30)     VALUE
                       "ｴﾗｰ=>1  2  3  4  5  6  7  8  9".
 01  HEAD03.
     03  FILLER         CHARACTER TYPE HYOUJUN.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"量販商品".
*        05  FILLER     PIC  X(02)     VALUE  "CD".
         05  FILLER     PIC  X(38)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"商品".
         05  FILLER     PIC  X(02)     VALUE  "CD".
         05  FILLER     PIC  X(03)     VALUE  SPACE.
         05  FILLER     PIC  N(01)     VALUE  NC"品".
         05  FILLER     PIC  X(04)     VALUE  SPACE.
         05  FILLER     PIC  N(01)     VALUE  NC"単".
         05  FILLER     PIC  X(07)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"原単価".
         05  FILLER     PIC  X(05)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"売単価".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"摘　要".
         05  FILLER     PIC  X(26)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"備　考".
 01  HEAD04.
     03  FILLER         CHARACTER TYPE HYOUJUN.
         05  FILLER     PIC  X(13)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"店舗Ｃ".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"店舗Ｃ".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"店舗Ｃ".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"店舗Ｃ".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"店舗Ｃ".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"店舗Ｃ".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"店舗Ｃ".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"店舗Ｃ".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"店舗Ｃ".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"店舗Ｃ".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"店舗Ｃ".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"店舗Ｃ".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"店舗Ｃ".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"店舗Ｃ".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"店舗Ｃ".
 01  HEAD05.
     03  FILLER         CHARACTER TYPE HYOUJUN.
         05  FILLER     PIC  X(15)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"数量".
         05  FILLER     PIC  X(04)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"数量".
         05  FILLER     PIC  X(04)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"数量".
         05  FILLER     PIC  X(04)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"数量".
         05  FILLER     PIC  X(04)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"数量".
         05  FILLER     PIC  X(04)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"数量".
         05  FILLER     PIC  X(04)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"数量".
         05  FILLER     PIC  X(04)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"数量".
         05  FILLER     PIC  X(04)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"数量".
         05  FILLER     PIC  X(04)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"数量".
         05  FILLER     PIC  X(04)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"数量".
         05  FILLER     PIC  X(04)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"数量".
         05  FILLER     PIC  X(04)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"数量".
         05  FILLER     PIC  X(04)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"数量".
         05  FILLER     PIC  X(04)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"数量".
*
 01  MEIS01.
     03  FILLER         CHARACTER TYPE HYOUJUN.
         05  FILLER     PIC  X(01).
         05  ME-03      PIC  9(08).
         05  FILLER     PIC  X(01).
         05  ME-04      PIC  N(10).
         05  FILLER     PIC  X(01).
         05  ME-05      PIC  ZZZ9.
         05  FILLER     PIC  X(01).
         05  ME-052     PIC  ZZZ9.
         05  FILLER     PIC  X(02).
         05  ME-06      PIC  X(02).
         05  FILLER     PIC  X(03).
         05  ME-07      PIC  X(02).
         05  FILLER     PIC  X(02).
         05  ME-08      PIC  X(07).
         05  FILLER     PIC  X(02).
         05  ME-09      PIC  9(08).
         05  FILLER     PIC  X(01).
         05  ME-10      PIC  9(08).
         05  FILLER     PIC  X(01).
         05  ME-11      PIC  X(04).
         05  FILLER     PIC  X(02).
         05  ME-12      PIC  X(02).
         05  FILLER     PIC  X(02).
         05  ME-13      PIC  X(02).
         05  FILLER     PIC  X(02).
         05  ME-14      PIC  9(01).
         05  FILLER     PIC  X(02).
         05  ME-101     PIC  X(02).
         05  FILLER     PIC  X(01).
         05  ME-102     PIC  N(05).
         05  FILLER     PIC  X(02).
         05  ER-01      PIC  X(01).
         05  FILLER     PIC  X(02).
         05  ER-02      PIC  X(01).
         05  FILLER     PIC  X(02).
         05  ER-03      PIC  X(01).
         05  FILLER     PIC  X(02).
         05  ER-04      PIC  X(01).
         05  FILLER     PIC  X(02).
         05  ER-05      PIC  X(01).
         05  FILLER     PIC  X(02).
         05  ER-06      PIC  X(01).
         05  FILLER     PIC  X(02).
         05  ER-07      PIC  X(01).
         05  FILLER     PIC  X(02).
         05  ER-08      PIC  X(01).
         05  FILLER     PIC  X(02).
         05  ER-09      PIC  X(01).
 01  MEIS02.
     03  FILLER.
         05  FILLER     PIC  X(01).
         05  ME-15      PIC  X(13).
         05  FILLER     PIC  X(01).
         05  ME-16      PIC  X(15).
         05  ME-17      PIC  X(15).
         05  FILLER     PIC  X(02).
         05  ME-18      PIC  X(08).
         05  FILLER     PIC  X(01).
         05  ME-19      PIC  X(08).
*        05  FILLER     PIC  X(01).
*        05  ME-19      PIC  X(05).
*        05  FILLER     PIC  X(01).
*        05  ME-20      PIC  X(02).
*        05  FILLER     PIC  X(01).
*        05  ME-21      PIC  X(01).
         05  FILLER     PIC  X(01).
         05  ME-22      PIC  ZZ,ZZZ,ZZ9.99.
         05  FILLER     PIC  X(01).
         05  ME-23      PIC  ZZ,ZZZ,ZZ9.
         05  FILLER     PIC  X(01).
         05  ME-24      PIC  X(15).
*        05  FILLER     PIC  X(01).
         05  ME-25      PIC  X(15).
         05  FILLER     PIC  X(02).
         05  ME-26      PIC  X(10).
 01  MEIS03.
     03  ME-27A.
             07  FILLER      PIC  X(04).
     03  FILLER              OCCURS    16.
         05  ME-27X.
             07  FILLER      PIC  X(01).
             07  ME-27L      PIC  X(01).
             07  ME-27       PIC  ZZZZ9.
             07  ME-27R      PIC  X(01).
         05  ME-27Y          REDEFINES      ME-27X.
             07  FILLER      PIC  X(02).
             07  ME-271      PIC  X(05).
             07  ME-271R     PIC  X(01).
 01  MEIS04.
     03  ME-28A.
             07  FILLER      PIC  X(04).
             07  ME-28SA     PIC  ZZZZZZ9.
             07  FILLER      PIC  X(01).
     03  FILLER          REDEFINES      ME-28A.
             07  FILLER      PIC  X(03).
             07  ME-281A     PIC  ZZZ9.
             07  ME-282A     PIC  ZZZ9.
             07  FILLER      PIC  X(01).
     03  FILLER              OCCURS    15.
         05  ME-28X.
             07  ME-28       PIC  ZZZZZZ9.
             07  FILLER      PIC  X(01).
 01  MEIS05.
     03  J-LINE-X       PIC  X(136)    VALUE  ALL  "=".
 01  P-SPACE            PIC  X(01)     VALUE  SPACE.
 01  P-LINE             PIC  X(136)    VALUE  ALL  "-".
 01  P-LINE2.
     03  FILLER         PIC  X(12)     VALUE  SPACE.
     03  FILLER         PIC  X(124)    VALUE  ALL  "-".
*
*----<< ﾀﾝﾄｳｼｬﾏｽﾀ ﾌﾞﾓﾝCD >>--*
 01  TAN-BMN            PIC   X(04)    VALUE  "2920".
*
 LINKAGE                     SECTION.
 01  PARA-BUMON             PIC   X(04).
 01  PARA-TANCD             PIC   X(02).
 01  PARA-MEMOST            PIC   9(04).
 01  PARA-MEMOEN            PIC   9(04).
****************************************************************
 PROCEDURE             DIVISION  USING  PARA-BUMON  PARA-TANCD
                                 PARA-MEMOST  PARA-MEMOEN.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 量販店ＪＮＬ >>--*
 PCRYOJF-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      PCRYOJF.
     ACCEPT   WK-DATE        FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SPD0115L PCRYOJF ERROR " PCRYOJF-ST " "
              WK-Y "." WK-M "." WK-D " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
*
     CLOSE    PCRYOJF   HTOKMS    HSHOTBL    HTANMS.
*
     STOP     RUN.
*----<< 取引先マスタ >>--*
 HTOKMS-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HTOKMS.
     ACCEPT   WK-DATE        FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SPD0115L HTOKMS ERROR " HTOKMS-ST " "
              WK-Y "." WK-M "." WK-D " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
*
     CLOSE    PCRYOJF   HTOKMS    HSHOTBL    HTANMS.
*
     STOP     RUN.
*----<< 商品変換テーブル >>--*
 HSHOTBL-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HSHOTBL.
     ACCEPT   WK-DATE        FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SPD0115L HSHOTBL ERROR " HSHOTBL-ST " "
              WK-Y "." WK-M "." WK-D " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
*
     CLOSE    PCRYOJF   HTOKMS    HSHOTBL    HTANMS.
*
     STOP     RUN.
*
*----<< 担当者マスタ >>--*
 HTANMS-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HTANMS.
     ACCEPT   WK-DATE        FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SPD0115L HTOKMS ERROR " HTANMS-ST " "
              WK-Y "." WK-M "." WK-D " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     CLOSE    PCRYOJF   HTOKMS    HSHOTBL    HTANMS.
     STOP     RUN.
*
 END DECLARATIVES.
*--------------------------------------------------------------*
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 000-PROG-CNTL          SECTION.
     PERFORM  100-INIT-RTN.
     PERFORM  200-MAIN-RTN   UNTIL     GR-NO    =    99.
     PERFORM  300-END-RTN.
     STOP RUN.
 000-PROG-CNTL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｼｮｷ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 100-INIT-RTN           SECTION.
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
     MOVE      LINK-OUT-YMD       TO   SYS-DATE.
*画面表示日付編集
     MOVE      SYS-DATE(1:4)      TO   HEN-DATE-YYYY.
     MOVE      SYS-DATE(5:2)      TO   HEN-DATE-MM.
     MOVE      SYS-DATE(7:2)      TO   HEN-DATE-DD.
*システム時刻取得
     ACCEPT    SYS-TIME         FROM   TIME.
*画面表示時刻編集
     MOVE      SYS-TIME(1:2)      TO   HEN-TIME-HH.
     MOVE      SYS-TIME(3:2)      TO   HEN-TIME-MM.
     MOVE      SYS-TIME(5:2)      TO   HEN-TIME-SS.
*
     DISPLAY  "*** SPD0115L START *** "
              WK-Y "." WK-M "." WK-D " "
              SYS-HH ":" WK-M ":" SYS-SS UPON CONS.
     OPEN     I-O       PCRYOJF.
     OPEN     INPUT     HTOKMS.
     OPEN     INPUT     HSHOTBL.
*
     OPEN     INPUT     HTANMS.
*
     MOVE     PARA-MEMOST         TO    ST-MEMO.
     MOVE     PARA-MEMOEN         TO    ED-MEMO.
*
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
*----<< ｲﾝｻﾂ >>-*
     PERFORM  240-PRINT.
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
     CLOSE    PCRYOJF.
     CLOSE    HTOKMS.
     CLOSE    HSHOTBL.
*
     CLOSE    HTANMS.
*
*
     ACCEPT   WK-DATE        FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SPD0115L END *** "
              WK-Y "." WK-M "." WK-D " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ｲﾝｻﾂ                                        *
*--------------------------------------------------------------*
 240-PRINT              SECTION.
     OPEN     OUTPUT    PRTF.
     MOVE     99             TO   LINE-CNT.
     MOVE     0              TO   PAGE-CNT.
     MOVE     LOW-VALUE      TO   BREAK-KEY.
     INITIALIZE              GOKEI-AREA.
     MOVE     0              TO   IX.
     MOVE     SPACE          TO   MEIS03.
     MOVE     SPACE          TO   MEIS04.
*
     MOVE     PARA-MEMOST    TO   PCR-F011.
     MOVE     ZERO           TO   PCR-F012.
     MOVE     ZERO           TO   PCR-F02.
     PERFORM  900-PCR-START-READ.
     PERFORM  241-LIST-PRINT
                        UNTIL     OLD  =    HIGH-VALUE.
     CLOSE    PRTF.
*処理を終了する。
     MOVE     99             TO   GR-NO.
 240-PRINT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ｲﾝｻﾂ                                        *
*--------------------------------------------------------------*
 241-LIST-PRINT         SECTION.
*    印字するべき店舗Ｃ・数量が無い場合
     IF       OLD  NOT  =    LOW-VALUE
     AND      NEW  NOT  =    OLD
              MOVE      1  TO   SEN-FLG
              PERFORM   2412-MEIS03-PRINT
     END-IF.
*
*    ２つ目からの取引先の明細部
     IF       NEW  NOT  =    HIGH-VALUE
     AND      NEW  NOT  =    OLD
              PERFORM   2411-MEIS01-SET
              PERFORM   2411-MEIS01-PRINT
              ADD       1              TO   IX
              MOVE      "ﾒﾓ NO."       TO   ME-271(IX)
     END-IF.
*
     MOVE     NEW            TO   OLD.
*    MOVE     NEW-MEMO02     TO   OLD-MEMO02.
     IF       NEW  NOT  =    HIGH-VALUE
              PERFORM   2413-MEIS-SET
              PERFORM   900-PCR-READ
     END-IF.
 241-LIST-PRINT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4     ﾒｲｻｲ ｲﾝｻﾂ                                    *
*--------------------------------------------------------------*
 2411-MEIS01-SET        SECTION.
*転送（ＭＥＩＳ０１）
     MOVE     SPACE          TO   MEIS01.
     MOVE     PCR-F11        TO   ME-03.
     MOVE     PCR-F11        TO   TOK-F01.
     PERFORM  900-TOK-READ.
     MOVE     TOK-F03        TO   ME-04.
     MOVE     PCR-F011       TO   ME-05.
     MOVE     PCR-F012       TO   ME-052.
     MOVE     PCR-F04        TO   ME-06.
     MOVE     PCR-F05        TO   ME-07.
     MOVE     PCR-F06        TO   ME-08.
     MOVE     PCR-F071       TO   ME-09.
     MOVE     PCR-F072       TO   ME-10.
     MOVE     PCR-F08        TO   ME-11.
     MOVE     PCR-F09        TO   ME-12.
     MOVE     PCR-F10        TO   ME-13.
     MOVE     PCR-F60        TO   ME-14.
*
     MOVE     PARA-BUMON          TO   TAN-F01.
*##2016/08/17 NAV ST TAKAHASHI
*****MOVE     PCR-F03             TO   TAN-F02.
     MOVE     PARA-TANCD          TO   TAN-F02.
*##2016/08/17 NAV ED TAKAHASHI
     PERFORM  900-TAN-READ.
*##2016/08/17 NAV ST TAKAHASHI
*****MOVE     PCR-F03             TO   ME-101.
     MOVE     PARA-TANCD          TO   ME-101.
*##2016/08/17 NAV ED TAKAHASHI
     MOVE     TAN-F03             TO   ME-102.
*
*転送（ＭＥＩＳ０２）
     MOVE     SPACE          TO   MEIS02.
     MOVE     PCR-F121       TO   ME-15.
     MOVE     PCR-F1211      TO   ME-16.
     MOVE     PCR-F1212      TO   ME-17.
     MOVE     PCR-F621       TO   ME-18.
     MOVE     PCR-F622       TO   ME-19.
*    MOVE     WK-0321        TO   ME-19.
*    MOVE     WK-0322        TO   ME-20.
*    MOVE     WK-0323        TO   ME-21.
     MOVE     PCR-F131       TO   ME-22.
     MOVE     PCR-F132       TO   ME-23.
     MOVE     PCR-F141       TO   ME-24.
     MOVE     PCR-F142       TO   ME-25.
     MOVE     PCR-F15        TO   ME-26.
**
**   エラー区分セット
     MOVE     PCR-F575       TO   ER-01.
     MOVE     PCR-F576       TO   ER-02.
     MOVE     PCR-F577       TO   ER-03.
     MOVE     PCR-F578       TO   ER-04.
     MOVE     PCR-F579       TO   ER-05.
     MOVE     PCR-F57A       TO   ER-06.
     MOVE     PCR-F57B       TO   ER-07.
     MOVE     PCR-F57C       TO   ER-08.
     MOVE     PCR-F57D       TO   ER-09.
**
 2411-MEIS01-SET-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4     ﾒｲｻｲ ｲﾝｻﾂ                                    *
*--------------------------------------------------------------*
 2411-MEIS01-PRINT      SECTION.
*改頁
     IF       LINE-CNT  >    57
       PERFORM   2414-HEAD-PRINT
     ELSE
*印刷
       WRITE    PRT-REC        FROM MEIS01    AFTER     1
       WRITE    PRT-REC        FROM MEIS02    AFTER     1
       WRITE    PRT-REC        FROM P-LINE    AFTER     1
       ADD      3              TO   LINE-CNT
*
*     P-LINE2を引くためのフラグを消す
      MOVE      ZERO           TO   WRT-FLG
*     MEIS04の先頭にメモ_の値を入れるためのフラグ立て
      MOVE      1              TO   MS4-FLG
     END-IF.
 2411-MEIS01-PRINT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4     ﾒｲｻｲ ｲﾝｻﾂ                                    *
*--------------------------------------------------------------*
 2412-MEIS03-PRINT      SECTION.
*改頁
*    IF       LINE-CNT  >    58
*             PERFORM   2414-HEAD-PRINT
*    END-IF.
*
*印字すべき数量が在る場合出力する
     IF       MEIS04  NOT =  SPACE
*
*改頁
       IF       LINE-CNT  >    59
                PERFORM   2414-HEAD-PRINT
       END-IF
*
*
       IF    WRT-FLG      =  1
         WRITE    PRT-REC      FROM P-LINE2   AFTER     1
       END-IF
*
       WRITE    PRT-REC        FROM MEIS03    AFTER     1
       WRITE    PRT-REC        FROM MEIS04    AFTER     1
*
       MOVE     1              TO   WRT-FLG
       ADD      3              TO   LINE-CNT
*
     END-IF.
*
     IF    SEN-FLG   =   1
       WRITE    PRT-REC      FROM P-LINE    AFTER     1
       MOVE     ZERO         TO   SEN-FLG
       MOVE     ZERO         TO   WRT-FLG
     END-IF.
*
     MOVE     SPACE          TO   MEIS03.
     MOVE     SPACE          TO   MEIS04.
     MOVE     0              TO   IX.
 2412-MEIS03-PRINT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4     ﾒｲｻｲ ｾｯﾄ                                     *
*--------------------------------------------------------------*
 2413-MEIS-SET          SECTION.
*店舗ＣＤ・数量項目を添え字１から５０まで明細にセットする
     PERFORM   24131-TBL-SET VARYING   I    FROM 1    BY   1
                             UNTIL     I    >    50.
 2413-MEIS-SET-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4     ﾒｲｻｲ ｾｯﾄ                                     *
*--------------------------------------------------------------*
 24131-TBL-SET          SECTION.
     IF       PCR-F162 (I)   NOT  =    ZERO
              ADD       1    TO   IX
              MOVE      "("            TO   ME-27L(IX)
              MOVE      PCR-F161 (I)   TO   ME-27 (IX)
              MOVE      ")"            TO   ME-27R(IX)
*明細２を印字した直後か判別しメモ_を明細４の先頭にセット
        IF     MS4-FLG = 1
                 MOVE      PCR-F011     TO   ME-281A
                 MOVE      PCR-F012     TO   ME-282A
                 MOVE      ZERO         TO   MS4-FLG
        END-IF
*明細４の行に数量をセット
        IF       IX = 1
           MOVE      PCR-F162(I)   TO   ME-28SA
        ELSE
           MOVE      PCR-F162(I)  TO   ME-28 (IX - 1)
        END-IF
     END-IF.
     IF       IX   =    16
              PERFORM   2412-MEIS03-PRINT
     END-IF.
 24131-TBL-SET-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL 4       ﾀｲﾄﾙ ﾌﾟﾘﾝﾄ ｼｮﾘ                              *
*--------------------------------------------------------------*
 2414-HEAD-PRINT        SECTION.
     IF       PAGE-CNT  =    0
              PERFORM   2411-MEIS01-SET
     ELSE
              WRITE     PRT-REC   FROM P-SPACE   AFTER  PAGE
     END-IF.
*
     ADD      1                   TO   PAGE-CNT.
     MOVE     SYS-DATE(1:4)       TO   HD-011.
     MOVE     SYS-DATE(5:2)       TO   HD-012.
     MOVE     SYS-DATE(7:2)       TO   HD-013.
     MOVE     PAGE-CNT            TO   HD-014.
     MOVE    "SPD0115L"           TO   HD-001.
*
     WRITE    PRT-REC   FROM      HEAD01    AFTER     2.
     WRITE    PRT-REC   FROM      WAKU1     AFTER     1.
     WRITE    PRT-REC   FROM      WAKU2     AFTER     1.
     WRITE    PRT-REC   FROM      WAKU3     AFTER     1.
     WRITE    PRT-REC   FROM      WAKU4     AFTER     1.
     WRITE    PRT-REC   FROM      WAKU5     AFTER     1.
     WRITE    PRT-REC   FROM      HEAD002   AFTER     1.
     WRITE    PRT-REC   FROM      MEIS05    AFTER     1.
     WRITE    PRT-REC   FROM      HEAD02    AFTER     1.
     WRITE    PRT-REC   FROM      HEAD03    AFTER     1.
     WRITE    PRT-REC   FROM      HEAD04    AFTER     1.
     WRITE    PRT-REC   FROM      HEAD05    AFTER     1.
     WRITE    PRT-REC   FROM      MEIS05    AFTER     1.
*行カウンタの初期化とヘッダ分の値をカウント
     MOVE     14             TO   LINE-CNT.
*
     PERFORM   2411-MEIS01-PRINT.
 2414-HEAD-PRINT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    量販店ＪＮＬ　 START READ                    *
*--------------------------------------------------------------*
 900-PCR-START-READ     SECTION.
     START    PCRYOJF   KEY  >=   PCR-F011  PCR-F012  PCR-F02
              INVALID   KEY
                        MOVE HIGH-VALUE     TO   NEW
     END-START.
*量販店ＪＮＬにスタートが掛けられた場合
     IF       NEW  NOT  =    HIGH-VALUE
              PERFORM   900-PCR-READ
     END-IF.
*
 900-PCR-START-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    量販店ＪＮＬ　 READ                          *
*--------------------------------------------------------------*
 900-PCR-READ           SECTION.
     READ     PCRYOJF   AT   END
              MOVE      HIGH-VALUE     TO   NEW
              GO   TO   900-PCR-READ-EXIT
     END-READ.
 READ-010.
     IF       PCR-F011  >    PARA-MEMOEN
              MOVE      HIGH-VALUE     TO   NEW
     END-IF.
     REWRITE  PCR-REC.
 READ-020.
*量販店ＪＮＬが読み込め、メモ_が範囲内の時
     IF       NEW       NOT  =    HIGH-VALUE
              MOVE      PCR-F011       TO   NEW-MEMO-1
              MOVE      PCR-F012       TO   NEW-MEMO-2
     END-IF.
 900-PCR-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    取引先マスタ　 READ                          *
*--------------------------------------------------------------*
 900-TOK-READ           SECTION.
     READ     HTOKMS    INVALID
              MOVE      SPACE          TO   TOK-F03
     END-READ.
 900-TOK-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    商品変換テーブル　　READ                     *
*--------------------------------------------------------------*
 900-SHO-READ           SECTION.
     READ     HSHOTBL    INVALID
              INITIALIZE          SHO-REC
     END-READ.
 900-SHO-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    担当者マスタ　READ                           *
*--------------------------------------------------------------*
 900-TAN-READ           SECTION.
     READ     HTANMS    INVALID
              MOVE      SPACE          TO   TAN-F03
     END-READ.
 900-TAN-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```
