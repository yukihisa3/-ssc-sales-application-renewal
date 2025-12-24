****************************************************************
*    顧客名　　　　　：　サカタのタネ（株）　　　　　　　　　　*
*    業務名　　　　　：　Ｄ３６５連携　　　　　　　　　　　　　*
*    モジュール名　　：　倉庫間出庫入力　　　　　　　　　　　　*
*    作成日／作成者　：　2020/04/20   ASS.II                   *
*    処理内容　　　　：　倉庫間在庫移動指示に従い出庫入力を行う*
*    作成日／作成者　：　2021/03/19   NAV TAKAHASHI            *
*    処理内容　　　　：　指示数＜出荷数の場合、エラーとする。　*
*    作成日／作成者　：　2021/04/14   NAV TAKAHASHI            *
*    処理内容　　　　：　ストックＮＯ内訳合計＝出荷数のチェック*
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            NVD0490I.
 AUTHOR.                ASS.II.
 DATE-WRITTEN.          20/04/20.
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       K-150SI.
 OBJECT-COMPUTER.       K-150SI.
 SPECIAL-NAMES.
     CONSOLE     IS     CONS
     STATION     IS     STAT.
***************************************************************
 INPUT-OUTPUT           SECTION.
***************************************************************
 FILE-CONTROL.
*----<< 新入出庫ファイル >>-*
     SELECT   DNSFILF   ASSIGN    TO        DA-01-VI-DNSFILL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      DYNAMIC
                        RECORD    KEY       DNS-F01  DNS-F02
                        FILE      STATUS    DNS-ST1.
*----<< 条件ファイル >>-*
     SELECT   HJYOKEN   ASSIGN    TO        DA-01-VI-JYOKEN1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       JYO-F01   JYO-F02
                        FILE      STATUS    JYO-ST1.
*----<< 倉庫マスタ >>-*
     SELECT   ZSOKMS    ASSIGN    TO        DA-01-VI-ZSOKMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       SOK-F01
                        FILE      STATUS    SOK-ST1.
*----<< 商品名称マスタ >>-*
     SELECT   HMEIMS    ASSIGN    TO        DA-01-VI-MEIMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      DYNAMIC
                        RECORD    KEY       MEI-F011   MEI-F0121
                                            MEI-F0122  MEI-F0123
                        FILE      STATUS    MEI-ST1.
*----<< 担当者マスタ >>-*
     SELECT   HTANMS    ASSIGN    TO        DA-01-VI-TANMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TAN-F01
                                            TAN-F02
                        FILE      STATUS    TAN-ST1.
*----<< 商品在庫マスタ >>-*
     SELECT   ZAMZAIF   ASSIGN    TO        DA-01-VI-ZAMZAIL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       ZAI-F01  ZAI-F02
                                            ZAI-F03
                        FILE      STATUS    ZAI-ST1.
*----<< 画面ファイル >>-*
     SELECT   DSPFILE   ASSIGN    TO        GS-DSPF
                        SYMBOLIC  DESTINATION        "DSP"
                        FORMAT              DSP-FMT
                        GROUP               DSP-GRP
                        PROCESSING  MODE    DSP-PRO
                        SELECTED  FUNCTION  DSP-FNC
                        FILE      STATUS    DSP-ST1.
******************************************************************
 DATA                      DIVISION.
******************************************************************
 FILE                      SECTION.
*----<< 新入出庫ファイル >>-*
 FD  DNSFILF            LABEL     RECORD     IS  STANDARD.
     COPY     DNSFILF   OF   XFDLIB    JOINING   DNS  AS PREFIX.
*----<< 条件ファイル >>-*
 FD  HJYOKEN            LABEL     RECORD     IS  STANDARD.
     COPY     JYOKEN1   OF   XFDLIB    JOINING   JYO  AS PREFIX.
*----<< 倉庫マスタ >>-*
 FD  ZSOKMS             LABEL     RECORD     IS  STANDARD.
     COPY     ZSOKMS1   OF   XFDLIB    JOINING   SOK  AS PREFIX.
*----<< 商品名称マスタ >>-*
 FD  HMEIMS             LABEL     RECORD     IS  STANDARD.
     COPY     MEIMS1    OF   XFDLIB    JOINING   MEI  AS PREFIX.
*----<< 担当者マスタ >>-*
 FD  HTANMS             LABEL     RECORD     IS  STANDARD.
     COPY     HTANMS    OF   XFDLIB    JOINING   TAN  AS PREFIX.
*----<< 商品在庫マスタ >>-*
 FD  ZAMZAIF            LABEL     RECORD     IS  STANDARD.
     COPY     ZAMZAIF   OF   XFDLIB    JOINING   ZAI  AS PREFIX.
*----<< 画面ファイル >>-*
 FD  DSPFILE.
     COPY     FVD04901  OF   XMDLIB.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  PGM-ID                  PIC  X(08)     VALUE  "NVD0490I".
 01  DATE-FLG                PIC  9(01)     VALUE   ZERO.
*\\
 01  ZAI-SIME.
     03  ZAI-SIME1           PIC  9(06)     VALUE ZERO.
     03  ZAI-SIME1R          REDEFINES      ZAI-SIME1.
         05  ZAI-SIME1R1     PIC  9(04).
         05  ZAI-SIME1R2     PIC  9(02).
     03  ZAI-SIME2           PIC  9(02)     VALUE ZERO.
*
 01  ZAI-SIMER               REDEFINES ZAI-SIME
                             PIC  9(08).
*----<< ﾃﾞｨｽﾌﾟﾚｲ ｺﾝﾄﾛｰﾙ ｴﾘｱ >>-*
 01  DSP-CNTL.
     03  DSP-FMT             PIC  X(08).
     03  DSP-GRP             PIC  X(08).
     03  DSP-PRO             PIC  X(02).
     03  DSP-FNC             PIC  X(04).
     03  DSP-ST1             PIC  X(02).
     03  DSP-ST2             PIC  X(04).
     03  DSP-CON             PIC  X(06).
     03  WK-GRP.
         05  WK-BODY             PIC  X(04).
         05  WK-LINE             PIC  9(02).
         05  FILLER              PIC  X(02).
 01  STATUS-AREA.
     03  IN-DATA             PIC  X(01).
     03  DNS-STATUS.
         05  DNS-ST1         PIC  X(02).
         05  DNS-ST2         PIC  X(04).
     03  ZAI-STATUS.
         05  ZAI-ST1         PIC  X(02).
         05  ZAI-ST2         PIC  X(04).
     03  JYO-STATUS.
         05  JYO-ST1         PIC  X(02).
         05  JYO-ST2         PIC  X(04).
     03  MEI-STATUS.
         05  MEI-ST1         PIC  X(02).
         05  MEI-ST2         PIC  X(04).
     03  SOK-STATUS.
         05  SOK-ST1         PIC  X(02).
         05  SOK-ST2         PIC  X(04).
     03  TAN-STATUS.
         05  TAN-ST1         PIC  X(02).
         05  TAN-ST2         PIC  X(04).
*画面退避用
     COPY   FVD04901  OF XMDLIB  JOINING   SAV  AS   PREFIX.
 01  TABLE-AREA.
     03  TABLE-G.
       05  TABLE1      OCCURS  2.
           07  TABLE2  OCCURS  4.
               09  TBL-GYONO        PIC   9(01).
               09  TBL-SYOCD        PIC   X(08).
               09  TBL-HIN1         PIC   X(05).
               09  TBL-HIN2         PIC   X(02).
               09  TBL-HIN3         PIC   X(01).
               09  TBL-SIJISU       PIC   9(07)V9(02).
               09  TBL-STAN1        PIC   X(01).
               09  TBL-STAN2        PIC   X(03).
               09  TBL-STAN3        PIC   X(02).
               09  TBL-SYKSU        PIC   9(07)V9(02).
               09  TBL-BIKOU        PIC   X(10).
               09  TBL-STK1         PIC   X(06).
               09  TBL-STK2         PIC   X(06).
               09  TBL-STK3         PIC   X(06).
               09  TBL-STK4         PIC   X(06).
               09  TBL-STK5         PIC   X(06).
               09  TBL-STS1         PIC   9(06).
               09  TBL-STS2         PIC   9(06).
               09  TBL-STS3         PIC   9(06).
               09  TBL-STS4         PIC   9(06).
               09  TBL-STS5         PIC   9(06).
     03  TABLE-G-R  REDEFINES TABLE-G.
         05  TABLE2-R  OCCURS  8.
               09  TBL-GYONO-R      PIC   9(01).
               09  TBL-SYOCD-R      PIC   X(08).
               09  TBL-HIN1-R       PIC   X(05).
               09  TBL-HIN2-R       PIC   X(02).
               09  TBL-HIN3-R       PIC   X(01).
               09  TBL-SIJISU-R     PIC   9(07)V9(02).
               09  TBL-STAN1-R      PIC   X(01).
               09  TBL-STAN2-R      PIC   X(03).
               09  TBL-STAN3-R      PIC   X(02).
               09  TBL-SYKSU-R      PIC   9(07)V9(02).
               09  TBL-BIKOU-R      PIC   X(10).
               09  TBL-STK1-R       PIC   X(06).
               09  TBL-STK2-R       PIC   X(06).
               09  TBL-STK3-R       PIC   X(06).
               09  TBL-STK4-R       PIC   X(06).
               09  TBL-STK5-R       PIC   X(06).
               09  TBL-STS1-R       PIC   9(06).
               09  TBL-STS2-R       PIC   9(06).
               09  TBL-STS3-R       PIC   9(06).
               09  TBL-STS4-R       PIC   9(06).
               09  TBL-STS5-R       PIC   9(06).
*----<< ﾜｰｸ ｴﾘｱ >>----
 01  WK-AREA.
     03  WK-BSYNO1           PIC  X(02)  VALUE  SPACE.
     03  WK-BSYNO2           PIC  X(02)  VALUE  SPACE.
     03  WK-SIME             PIC  9(08)  VALUE  ZERO.
 01  WK-DPNO-AREA.
     03  WK-DPNO             PIC  9(07)  VALUE  ZERO.
 01  WK-HINMEI.
     03  WK-HINMEI1          PIC  N(15).
     03  WK-HINMEI2          PIC  N(15).
 01  WK-GYONO.
     03  WK-NUM1             PIC  N(01)  VALUE  NC"１".
     03  WK-NUM2             PIC  N(01)  VALUE  NC"２".
     03  WK-NUM3             PIC  N(01)  VALUE  NC"３".
     03  WK-NUM4             PIC  N(01)  VALUE  NC"４".
     03  WK-NUM5             PIC  N(01)  VALUE  NC"５".
     03  WK-NUM6             PIC  N(01)  VALUE  NC"６".
     03  WK-NUM7             PIC  N(01)  VALUE  NC"　".
     03  WK-NUM8             PIC  N(01)  VALUE  NC"　".
 01  FILLER                  REDEFINES   WK-GYONO.
     03  GYO-TBL             PIC  N(01)  OCCURS  8.
*----<< ﾃﾞｨｽﾌﾟﾚｲ ｶﾞｲﾀﾞﾝｽ ｴﾘｱ >>-*
 01  GUIDE-AREA.
     03  G001                PIC  N(30)  VALUE
         NC"_取消_終了".
     03  G002                PIC  N(30)  VALUE
         NC"_取消_終了_項目戻り".
     03  G003                PIC  N(30)  VALUE
         NC"_取消_終了_項目戻り_前頁_次頁".
 01  MSG-AREA.
     03  MSG01               PIC  N(20)  VALUE
                    NC"表示中のＰＦキー以外は使用できません".
     03  MSG02               PIC  N(20)  VALUE
                    NC"該当伝票番号がファイルに存在しません".
     03  MSG03               PIC  N(20)  VALUE
                    NC"伝票区分に誤りがあります．".
     03  MSG04               PIC  N(20)  VALUE
                    NC"入出庫日の入力に誤りがあります．".
     03  MSG05               PIC  N(20)  VALUE
                    NC"場所に誤りがあります．".
     03  MSG06               PIC  N(20)  VALUE
                    NC"数量に誤りがあります．".
     03  MSG07               PIC  N(20)  VALUE
                    NC"Ｙを入力して下さい．".
     03  MSG08               PIC  N(20)  VALUE
                  NC"入力された伝票_は既に登録されています．".
     03  MSG09               PIC  N(20)  VALUE
                    NC"入力された場所は存在しません．".
     03  MSG10               PIC  N(20)  VALUE
                    NC"該当伝票番号は存在しません".
     03  MSG11               PIC  N(20)  VALUE
                    NC"削除区分に誤りがあります．".
     03  MSG12               PIC  N(20)  VALUE
                    NC"＊＊＊＊＊＊＊＊＊＊＊＊＊".
     03  MSG13               PIC  N(20)  VALUE
                    NC"次レコードは存在しません．".
     03  MSG14               PIC  N(20)  VALUE
                    NC"前レコードは存在しません．".
     03  MSG15               PIC  N(20)  VALUE
                    NC"処理区分に誤りがあります．".
     03  MSG16               PIC  N(20)  VALUE
                    NC"該当伝票番号は既に連携済です".
     03  MSG17               PIC  N(20)  VALUE
                    NC"明細を入力して下さい．".
     03  MSG18               PIC  N(20)  VALUE
                    NC"在庫マスタに存在しません（続行可能）".
     03  MSG19               PIC  N(20)  VALUE
                    NC"担当者に誤りがあります．".
     03  MSG20               PIC  N(20)  VALUE
                    NC"未出庫フラグに誤りがあります".
     03  MSG21               PIC  N(20)  VALUE
                    NC"次頁はありません".
     03  MSG22               PIC  N(20)  VALUE
                    NC"前頁はありません".
     03  MSG23               PIC  N(20)  VALUE
                    NC"次頁に誤りがあります".
     03  MSG24               PIC  N(20)  VALUE
                    NC"ＳＴＮＯ合計と実出荷数が合いません！！".
     03  MSG25               PIC  N(20)  VALUE
                    NC"指示数を超えています！！".
 01  FILLER                  REDEFINES   MSG-AREA.
     03  MSG-TBL             PIC  N(20)  OCCURS       25.
*----<< ｼｽﾃﾑ ﾋﾂﾞｹ･ｼﾞｶﾝ ｴﾘｱ >>----
 01  WK-SYS-DATE             PIC  9(08).
*
 01  WK-SYS-YMD              PIC  9(06).
 01  WK-SYS-DATER    REDEFINES    WK-SYS-YMD.
     03  WK-SYS-YYR          PIC  9(02).
     03  WK-SYS-MMR          PIC  9(04).
     03  WK-SYS-DDR          PIC  9(04).
 01  WK-NYSYMD.
     03  WK-NYSYMD1          PIC  9(02).
     03  WK-NYSYMD2          PIC  9(02).
     03  WK-NYSYMD3          PIC  9(02).
 01  WK-NYS-DATE             PIC  9(08).
 01  SYS-DATE                PIC  9(06).
 01  FILLER                  REDEFINES  SYS-DATE.
     03  SYS-YY              PIC  9(02).
     03  SYS-MM              PIC  9(02).
     03  SYS-DD              PIC  9(02).
 01  SYS-DATE2               PIC  9(08).
 01  FILLER                  REDEFINES  SYS-DATE2.
     03  SYS-YYYY.
         05  SYS-YY2-1       PIC  9(02).
         05  SYS-YY2-2       PIC  9(02).
     03  SYS-MM2             PIC  9(02).
     03  SYS-DD2             PIC  9(02).
 01  SYS-TIME.
     03  SYS-HH              PIC  9(02).
     03  SYS-MN              PIC  9(02).
     03  SYS-SS              PIC  9(02).
     03  FILLER              PIC  9(02).
*----<< ｲﾝﾃﾞｯｸｽ >>----
 01  INDEXES.
     03  I                   PIC  9(02).
     03  J                   PIC  9(02).
     03  L                   PIC  9(02).
     03  IDX1                PIC  9(02).
     03  IXB                 PIC  9(02).
     03  IXC                 PIC  9(02).
     03  IXD                 PIC  9(02).
     03  IXE                 PIC  9(02).
*----<< ﾌﾗｸﾞ ｴﾘｱ >>----
 01  FLAGS.
     03  END-FLG             PIC  X(03)  VALUE  SPACE.
     03  INV-FLG             PIC  9(01)  VALUE  ZERO.
     03  DNS-FLG             PIC  9(01)  VALUE  ZERO.
     03  ERR-FLG             PIC  9(02)  VALUE  ZERO.
     03  SYR-FLG             PIC  9(02)  VALUE  ZERO.
     03  CHK-FLG             PIC  9(01)  VALUE  ZERO.
     03  ZAI-FLG             PIC  9(01).
     03  HJYOKEN-INV-FLG     PIC  X(03)  VALUE  SPACE.
*----<< ｶｳﾝﾄ ｴﾘｱ >>----
 01  COUNTERS.
     03  MAX-LINE            PIC  9(02)  VALUE  ZERO.
     03  GYO-CNT             PIC  9(02)  VALUE  ZERO.
     03  P-CNT               PIC  9(02)  VALUE  ZERO.
     03  P-MAX               PIC  9(02)  VALUE  ZERO.
     03  C-CNT               PIC  9(02)  VALUE  ZERO.
*----<< ﾋﾝﾀﾝ ﾜｰｸ ｴﾘｱ >>----
 01  WK-HINT-AREA.
     03  WK-HINTI-X.
         05  WK-HINTI        PIC  X(01)  OCCURS  5.
     03  WK-HINTO-X.
         05  WK-HINTO        PIC  X(01)  OCCURS  5.
     03  WK-HINT             PIC  X(05).
*----<< ｷｰ ﾜｰｸ ｴﾘｱ >>----
 01  WK-KEY-AREA.
     03  WK-F01              PIC  9(07).
     03  WK-ZAI-F01          PIC  X(02).
     03  WK-SOK              PIC  X(02).
*
*****日付入力許容範囲（年月日）
 01  WK-HANI-ARE.
     03  WK-HANI1            PIC  9(08)  VALUE  ZERO.
     03  WK-HANI2            PIC  9(08)  VALUE  ZERO.
*****日付入力許容範囲（年月日）
 01  WK-HENKAN.
     03  WK-HENKAN-1         PIC  9(04)  VALUE  ZERO.
     03  WK-HENKAN-2         PIC  9(02)  VALUE  ZERO.
     03  WK-HENKAN-3         PIC  9(02)  VALUE  ZERO.
*****年チェック
 01  WK-CHKS-DATE.
     03  WK-CHKS-YYYY        PIC  9(04)  VALUE  ZERO.
     03  WK-CHKS-MM          PIC  9(02)  VALUE  ZERO.
     03  WK-CHKS-DD          PIC  9(02)  VALUE  ZERO.
*****年チェック
 01  WK-CHKN-DATE.
     03  WK-CHKN-YYYY        PIC  9(04)  VALUE  ZERO.
     03  WK-CHKN-MM          PIC  9(02)  VALUE  ZERO.
     03  WK-CHKN-DD          PIC  9(02)  VALUE  ZERO.
*****在庫締年月
 01  WK-ZAIKO-SIME           PIC  9(09).
 01  FILLER                  REDEFINES   WK-ZAIKO-SIME.
     03  WK-ZAIKO-SIME-0     PIC  9(01).
     03  WK-ZAIKO-SIME-1     PIC  9(04).
     03  WK-ZAIKO-SIME-2     PIC  9(02).
     03  WK-ZAIKO-SIME-3     PIC  9(02).
*****日付入力許容範囲（月）
 01  WK-H-TUKI.
     03  WK-H-TUKI-1         PIC  9(02)  VALUE  ZERO.
     03  WK-H-TUKI-2         PIC  9(02)  VALUE  ZERO.
*
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
*特販部名称編集
 01  HEN-TOKHAN-AREA.
     03  FILLER                   PIC  N(01)  VALUE  NC"（".
     03  HEN-TOKHAN               PIC  N(06)  VALUE  SPACE.
     03  FILLER                   PIC  N(01)  VALUE  NC"）".
*****倉庫＋部門保存
 01  WK-SOKCD                     PIC  X(02)  VALUE  SPACE.
 01  WK-BUMON-CD                  PIC  9(04)  VALUE  ZERO.
 01  HEN-BUMON-CD                 PIC  9(04)  VALUE  ZERO.
*2021/04/14 NAV ST
 01  WK-STNO-GK                   PIC  9(07)  VALUE  ZERO.
 01  WK-STNO-SU1                  PIC  9(06)  VALUE  ZERO.
 01  WK-STNO-SU2                  PIC  9(06)  VALUE  ZERO.
 01  WK-STNO-SU3                  PIC  9(06)  VALUE  ZERO.
 01  WK-STNO-SU4                  PIC  9(06)  VALUE  ZERO.
 01  WK-STNO-SU5                  PIC  9(06)  VALUE  ZERO.
*2021/04/14 NAV ED
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
 01  OUT-DENNO             PIC 9(07).
*---<< ｻﾌﾞﾙｰﾁﾝ LINK AREA >>-*
 LINKAGE        SECTION.
 01  LINK-BUMON            PIC X(04).
 01  LINK-TANCD            PIC X(02).
*
******************************************************************
 PROCEDURE           DIVISION    USING  LINK-BUMON LINK-TANCD.
******************************************************************
*--------------------------------------------------------------*
*    LEVEL   0     エラー処理　　　　　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 DECLARATIVES.
*----------  新入出庫ファイル  --------------------------------*
 NYU-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE   DNSFILF.
     ACCEPT      SYS-DATE    FROM  DATE.
     ACCEPT      SYS-TIME    FROM  TIME.
     DISPLAY  "### " PGM-ID " " NC"新入出庫ファイル異常！"
              "ST1=" DNS-ST1                 " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"  UPON STAT.
*
     ACCEPT   IN-DATA        FROM STAT.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
*----------  条件ファイル　---------------------------------*
 JYO-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE   HJYOKEN.
     ACCEPT      SYS-DATE    FROM  DATE.
     ACCEPT      SYS-TIME    FROM  TIME.
     DISPLAY  "### " PGM-ID " " NC"条件ファイル異常！"
              "ST1=" JYO-ST1                 " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"  UPON STAT.
*
     ACCEPT   IN-DATA        FROM STAT.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
*----------   倉庫マスタ　-----------------------------------*
 AOK-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE   ZSOKMS.
     ACCEPT      SYS-DATE    FROM  DATE.
     ACCEPT      SYS-TIME    FROM  TIME.
     DISPLAY  "### " PGM-ID " " NC"倉庫マスタ異常！"
              "ST1=" SOK-ST1                 " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"  UPON STAT.
*
     ACCEPT   IN-DATA        FROM STAT.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
*----------   商品名称マスタ　 --------------------------------*
 MEI-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE   HMEIMS.
     ACCEPT      SYS-DATE    FROM  DATE.
     ACCEPT      SYS-TIME    FROM  TIME.
     DISPLAY  "### " PGM-ID " " NC"商品名称マスタ異常！"
              "ST1=" MEI-ST1                 " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"  UPON STAT.
*
     ACCEPT   IN-DATA        FROM STAT.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
*----------   担当者マスタ     --------------------------------*
 MEI-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE   HTANMS.
     ACCEPT      SYS-DATE    FROM  DATE.
     ACCEPT      SYS-TIME    FROM  TIME.
     DISPLAY  "### " PGM-ID " " NC"担当者マスタ異常！"
              "ST1=" TAN-ST1                 " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"  UPON STAT.
*
     ACCEPT   IN-DATA        FROM STAT.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
*----------  商品在庫マスタ  ----------------------------------*
 ZAI-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE   ZAMZAIF.
     ACCEPT      SYS-DATE    FROM  DATE.
     ACCEPT      SYS-TIME    FROM  TIME.
     DISPLAY  "### " PGM-ID " " NC"商品在庫マスタ異常！"
              "ST1=" ZAI-ST1                 " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"  UPON STAT.
*
     ACCEPT   IN-DATA        FROM STAT.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
*----------    表示ファイル -----------------------------------*
 DSP-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE   DSPFILE.
     ACCEPT      SYS-DATE    FROM  DATE.
     ACCEPT      SYS-TIME    FROM  TIME.
     DISPLAY  "### " PGM-ID " " NC"表示ファイル異常！"
              "ST1=" DSP-ST1                 " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"  UPON STAT.
*
     ACCEPT   IN-DATA        FROM STAT.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
 END DECLARATIVES.
*--------------------------------------------------------------*
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 PROG-CNTL          SECTION.
*****************テスト
***  MOVE  "2920"   TO  LINK-BUMON.
***  MOVE  "31"     TO  LINK-TANCD.
     DISPLAY "LINK-BUMON=" LINK-BUMON UPON CONS.
     DISPLAY "LINK-TANCD=" LINK-TANCD UPON STAT.
*****************
     PERFORM  INIT-RTN.
*****************テスト
     DISPLAY "ZAI-SIME=" ZAI-SIME UPON STAT.
*****************
     PERFORM  MAIN-RTN   UNTIL     END-FLG  =  "END".
     PERFORM  END-RTN.
     STOP RUN.
 PROG-CNTL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｼｮｷ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 INIT-RTN           SECTION.
     ACCEPT      SYS-DATE    FROM  DATE.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     SYS-DATE            TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD       TO   SYS-DATE2.
     ACCEPT      SYS-TIME    FROM  TIME.
     DISPLAY  "*** " PGM-ID " START "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ***"  UPON STAT.
*画面日付・時刻編集
     MOVE      SYS-DATE2(1:4)     TO   HEN-DATE-YYYY.
     MOVE      SYS-DATE2(5:2)     TO   HEN-DATE-MM.
     MOVE      SYS-DATE2(7:2)     TO   HEN-DATE-DD.
     MOVE      SYS-TIME(1:2)      TO   HEN-TIME-HH.
     MOVE      SYS-TIME(3:2)      TO   HEN-TIME-MM.
     MOVE      SYS-TIME(5:2)      TO   HEN-TIME-SS.
*フラグ初期化
     INITIALIZE         FLAGS.
*ファイル　オープン
     OPEN     I-O       DNSFILF.
     OPEN     INPUT     HJYOKEN.
     OPEN     INPUT     HMEIMS.
     OPEN     INPUT     ZSOKMS.
     OPEN     INPUT     HTANMS.
     OPEN     I-O       ZAMZAIF.
     OPEN     I-O       DSPFILE.
*在庫締日取得
     MOVE      99            TO   JYO-F01.
     MOVE     SPACE          TO   JYO-F02.
     MOVE     "ZAI"          TO   JYO-F02.
     READ     HJYOKEN
          INVALID
              DISPLAY   "HJYOKEN INV KEY=99"  UPON CONS
              MOVE      "END"     TO   END-FLG
              GO   TO   INIT-EXIT
     END-READ.
     MOVE     JYO-F05        TO   ZAI-SIME1.
     ADD      1              TO   ZAI-SIME1.
     IF       ZAI-SIME1R2    >    12
              MOVE     1     TO   ZAI-SIME1R2
              ADD      1     TO   ZAI-SIME1R1
     END-IF.
     MOVE     31             TO   ZAI-SIME2.
     MOVE     JYO-F04        TO   WK-ZAIKO-SIME.
*
     MOVE     0                   TO   SYR-FLG.
 INIT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 MAIN-RTN           SECTION.
*
     EVALUATE    SYR-FLG
         WHEN    0      PERFORM   DSP-INIT-RTN
         WHEN    1      PERFORM   DSP-DPNO-RTN
         WHEN    2      PERFORM   DSP-GRP03-RTN
         WHEN    3      PERFORM   DSP-GRP04-RTN
         WHEN    4      PERFORM   DSP-BODY-RTN
         WHEN    90     PERFORM   DSP-KAKNIN-RTN
     END-EVALUATE.
 MAIN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 END-RTN            SECTION.
* 各ファイルをクローズする
     CLOSE              DNSFILF
                        ZAMZAIF
                        HJYOKEN
                        HMEIMS
                        ZSOKMS
                        HTANMS
                        DSPFILE.
*
     ACCEPT      SYS-DATE    FROM  DATE.
     ACCEPT      SYS-TIME    FROM  TIME.
     DISPLAY  "*** " PGM-ID " END   "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ***"  UPON STAT.
 END-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  ALL    ﾃﾞｨｽﾌﾟﾚｰ  ｼｮｷ ﾋｮｳｼﾞ                         *
*--------------------------------------------------------------*
 DSP-INIT-RTN           SECTION.
     INITIALIZE         COUNTERS.
     INITIALIZE         FLAGS.
     MOVE     SPACE          TO   FVD04901.
*行_セット
     PERFORM  VARYING  I  FROM  1  BY  1  UNTIL  I  >  4
              MOVE  GYO-TBL(I)     TO  GYONO(I)
     END-PERFORM.
*属性クリア
     PERFORM  CLR-HEAD-RTN.
     PERFORM  CLR-BODY-RTN.
     PERFORM  CLR-TAIL-RTN.
*
     MOVE     SPACE          TO   DSP-CNTL.
     MOVE     "FVD04901"     TO   DSP-FMT.
     MOVE     "ALLF"         TO   DSP-GRP.
* 伝票NOの入力を行う
     MOVE     1              TO   SYR-FLG.
 DSP-INIT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＧＲＰ００３　入力　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 DSP-GRP03-RTN       SECTION.
     MOVE     G002           TO   GUIDE.
     MOVE     "GRP002"       TO   WK-GRP.
     PERFORM  DSP-RD-RTN.
*
     EVALUATE DSP-FNC
     WHEN     "F004"
              MOVE      0         TO   SYR-FLG
     WHEN     "F005"
              MOVE      "END"     TO   END-FLG
     WHEN     "F006"
              MOVE      1         TO   SYR-FLG
              PERFORM  CLR-HEAD-RTN
     WHEN     "E000"
              PERFORM  CHK-GRP03-RTN
              IF     ERR-FLG   =  ZERO
                     MOVE   3     TO   SYR-FLG
              END-IF
     WHEN     OTHER
              MOVE      1         TO   ERR-FLG
     END-EVALUATE.
*
 DSP-GRP03-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＧＲＰ００４　入力　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 DSP-GRP04-RTN       SECTION.
     MOVE     G002           TO   GUIDE.
     MOVE     "GRP003"       TO   WK-GRP.
     PERFORM  DSP-RD-RTN.
*
     EVALUATE DSP-FNC
     WHEN     "F004"
              MOVE      0         TO   SYR-FLG
     WHEN     "F005"
              MOVE      "END"     TO   END-FLG
     WHEN     "F006"
              MOVE      2         TO   SYR-FLG
              PERFORM  CLR-HEAD-RTN
     WHEN     "E000"
              PERFORM  CLR-HEAD-RTN
              PERFORM  CHK-DATE-RTN
              IF     ERR-FLG   =   ZERO
                     MOVE   4        TO  SYR-FLG
              END-IF
     WHEN     OTHER
              MOVE      1         TO   ERR-FLG
     END-EVALUATE.
*
 DSP-GRP04-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3
*--------------------------------------------------------------*
 CHK-DATE-RTN        SECTION.
*
*入出庫日
     IF  SYKYMD  IS       NOT  NUMERIC
         MOVE    0         TO  SYKYMD
     END-IF.
*日付論理チェック
     MOVE     "2"                 TO   LINK-IN-KBN.
     MOVE     ZERO                TO   LINK-IN-YMD6.
     MOVE     SYKYMD              TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     IF       LINK-OUT-RET   NOT =  ZERO
         IF  ERR-FLG       =   0
             MOVE   4          TO   ERR-FLG
         END-IF
         MOVE   "C"        TO   EDIT-CURSOR  OF  SYKYMD
         MOVE   "R"        TO   EDIT-OPTION  OF  SYKYMD
         GO      TO        CHK-DATE-EXT
     END-IF.
*
     MOVE     SYKYMD              TO   WK-NYS-DATE.
*
     MOVE     SYS-DATE2           TO   WK-CHKS-DATE.
     MOVE     WK-NYS-DATE         TO   WK-CHKN-DATE.
     IF  (WK-CHKS-YYYY        NOT =    WK-CHKN-YYYY ) AND
         (WK-CHKS-YYYY - 1    NOT =    WK-CHKN-YYYY )
         IF  ERR-FLG       =   0
             MOVE      4       TO   ERR-FLG
         END-IF
         MOVE   "C"        TO   EDIT-CURSOR  OF  SYKYMD
         MOVE   "R"        TO   EDIT-OPTION  OF  SYKYMD
         GO      TO        CHK-DATE-EXT
     END-IF.
*
*締日取得
     MOVE     "99"           TO   JYO-F01.
     MOVE     SPACE          TO   JYO-F02.
     PERFORM  JYO-RD-RTN.
     IF  INV-FLG         =   1
         DISPLAY   "HJYOKEN INV KEY=99"  UPON STAT
         MOVE  "END"    TO   END-FLG
         GO    TO       CHK-DATE-EXT
     END-IF.
* 入力した入出庫日の対象となる締日をワークにセットする
     EVALUATE   SYKYMD(5:2)
         WHEN      01    MOVE  JYO-F04     TO   WK-SIME
         WHEN      02    MOVE  JYO-F05     TO   WK-SIME
         WHEN      03    MOVE  JYO-F06     TO   WK-SIME
         WHEN      04    MOVE  JYO-F07     TO   WK-SIME
         WHEN      05    MOVE  JYO-F08     TO   WK-SIME
         WHEN      06    MOVE  JYO-F09     TO   WK-SIME
         WHEN      07    MOVE  JYO-F10     TO   WK-SIME
         WHEN      08    MOVE  JYO-F11     TO   WK-SIME
         WHEN      09    MOVE  JYO-F12     TO   WK-SIME
         WHEN      10    MOVE  JYO-F12A    TO   WK-SIME
         WHEN      11    MOVE  JYO-F12B    TO   WK-SIME
         WHEN      12    MOVE  JYO-F12C    TO   WK-SIME
     END-EVALUATE.
*締日－１算出 2001/10/09  追加
     COMPUTE   WK-SIME  =  WK-SIME  -  1.
     MOVE      SYS-DATE2          TO   WK-SYS-DATE.
*
*****DISPLAY  "00 WK-SIME=" WK-SIME
     IF  WK-SYS-DATE   >   WK-SIME
         IF  ERR-FLG       =   0
             MOVE   4          TO   ERR-FLG
         END-IF
         MOVE   "C"        TO   EDIT-CURSOR  OF  SYKYMD
         MOVE   "R"        TO   EDIT-OPTION  OF  SYKYMD
     END-IF.
*****DISPLAY  "01 WK-SYS-DATE=" WK-SYS-DATE
*****DISPLAY  "02 WK-NYS-DATE=" WK-NYS-DATE
     IF  WK-SYS-DATE   <   WK-NYS-DATE
         IF  ERR-FLG       =   0
             MOVE   4          TO   ERR-FLG
         END-IF
         MOVE   "C"        TO   EDIT-CURSOR  OF  SYKYMD
         MOVE   "R"        TO   EDIT-OPTION  OF  SYKYMD
     END-IF.
*
*----<許容範囲月算出>
     MOVE   WK-ZAIKO-SIME-2  TO   WK-H-TUKI-1.
     COMPUTE WK-H-TUKI-2 = WK-ZAIKO-SIME-2  -  1.
     IF     WK-H-TUKI-2  =  ZERO
            MOVE   12        TO   WK-H-TUKI-2
     END-IF.
*----<許容範囲開始年月日>
     EVALUATE   WK-H-TUKI-1
         WHEN      01    MOVE  JYO-F04     TO   WK-HANI1
         WHEN      02    MOVE  JYO-F05     TO   WK-HANI1
         WHEN      03    MOVE  JYO-F06     TO   WK-HANI1
         WHEN      04    MOVE  JYO-F07     TO   WK-HANI1
         WHEN      05    MOVE  JYO-F08     TO   WK-HANI1
         WHEN      06    MOVE  JYO-F09     TO   WK-HANI1
         WHEN      07    MOVE  JYO-F10     TO   WK-HANI1
         WHEN      08    MOVE  JYO-F11     TO   WK-HANI1
         WHEN      09    MOVE  JYO-F12     TO   WK-HANI1
         WHEN      10    MOVE  JYO-F12A    TO   WK-HANI1
         WHEN      11    MOVE  JYO-F12B    TO   WK-HANI1
         WHEN      12    MOVE  JYO-F12C    TO   WK-HANI1
     END-EVALUATE.
*----<許容範囲終了年月日>
     EVALUATE   WK-H-TUKI-2
         WHEN      01    MOVE  JYO-F04     TO   WK-HANI2
         WHEN      02    MOVE  JYO-F05     TO   WK-HANI2
         WHEN      03    MOVE  JYO-F06     TO   WK-HANI2
         WHEN      04    MOVE  JYO-F07     TO   WK-HANI2
         WHEN      05    MOVE  JYO-F08     TO   WK-HANI2
         WHEN      06    MOVE  JYO-F09     TO   WK-HANI2
         WHEN      07    MOVE  JYO-F10     TO   WK-HANI2
         WHEN      08    MOVE  JYO-F11     TO   WK-HANI2
         WHEN      09    MOVE  JYO-F12     TO   WK-HANI2
         WHEN      10    MOVE  JYO-F12A    TO   WK-HANI2
         WHEN      11    MOVE  JYO-F12B    TO   WK-HANI2
         WHEN      12    MOVE  JYO-F12C    TO   WK-HANI2
     END-EVALUATE.
*
*****REWRITE  JYO-REC.
*
*----<許容範囲チェック>
     MOVE          WK-HANI2    TO          WK-HENKAN.
     IF  WK-NYS-DATE   <=   WK-HANI1
         MOVE      01          TO          WK-HENKAN-3
     ELSE
         ADD       1           TO          WK-HENKAN-2
         IF        WK-HENKAN-2  >  12
                   ADD    1    TO          WK-HENKAN-1
                   MOVE  01    TO          WK-HENKAN-2
         END-IF
         MOVE      01          TO          WK-HENKAN-3
     END-IF.
     MOVE          WK-HENKAN   TO          WK-HANI2.
*****DISPLAY  "03 WK-HANI2=" WK-HANI2
*****DISPLAY  "04 WK-NYS-DATE=" WK-NYS-DATE
     IF  ( WK-HANI2   <=  WK-NYS-DATE )
         CONTINUE
     ELSE
         IF  ERR-FLG       =   0
             MOVE   4          TO   ERR-FLG
         END-IF
         MOVE   "C"        TO   EDIT-CURSOR  OF  SYKYMD
         MOVE   "R"        TO   EDIT-OPTION  OF  SYKYMD
     END-IF.
*
 CHK-DATE-EXT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＧＲＰ００３　チェック　　　　　　　　　　　*
*--------------------------------------------------------------*
 CHK-GRP03-RTN      SECTION.
*属性クリア
     PERFORM  CLR-HEAD-RTN.
***** 入力チェック *****
*担当者追加
     MOVE   SPACE            TO  TANNM.
     IF     TANTO       =    SPACE
            IF  ERR-FLG            =  0
                MOVE    19        TO  ERR-FLG
            END-IF
            MOVE    "C"      TO  EDIT-CURSOR  OF  TANTO
            MOVE    "R"      TO  EDIT-OPTION  OF  TANTO
     ELSE
            MOVE    TANTO    TO  TAN-F02
            PERFORM TAN-READ-RTN
            IF   INV-FLG     =  ZERO
                 MOVE   TAN-F03  TO  TANNM
            ELSE
                 MOVE  "R"       TO  EDIT-OPTION OF TANTO
                 MOVE  "C"       TO  EDIT-CURSOR OF TANTO
                 IF     ERR-FLG  =  ZERO
                        MOVE 19  TO  ERR-FLG
                 END-IF
            END-IF

     END-IF.
*
*入出庫日
     IF     ERR-FLG  =  ZERO
            MOVE    3           TO   SYR-FLG
     END-IF.
     PERFORM   CHK-DATE-RTN.
*
 CHK-GRP03-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      伝票■　入力                                *
*--------------------------------------------------------------*
 DSP-DPNO-RTN     SECTION.
*
     MOVE     G001           TO   GUIDE.
     MOVE     "GRP001"       TO   WK-GRP.
     PERFORM  DSP-RD-RTN.
*アテンション判定
     EVALUATE DSP-FNC
     WHEN     "F004"
              MOVE    0           TO   SYR-FLG
     WHEN     "F005"
              MOVE    "END"       TO   END-FLG
     WHEN     "F006"
              MOVE    1           TO   SYR-FLG
              PERFORM  CLR-HEAD-RTN
              MOVE  ZERO          TO   DNS-FLG
     WHEN     "E000"
              MOVE   1            TO   P-CNT
              PERFORM   CHK-DPNO-RTN
              IF     ERR-FLG   =   ZERO
                     MOVE   2      TO   SYR-FLG
              ELSE
                     MOVE  ZERO    TO   DNS-FLG
              END-IF
     WHEN     "F011"
              MOVE   1            TO   P-CNT
              PERFORM   MAE-REC-RTN
              IF     ERR-FLG   =   ZERO
                     MOVE   1      TO   SYR-FLG
              END-IF
     WHEN     "F012"
              MOVE   1            TO   P-CNT
              PERFORM   TUGI-REC-RTN
              IF     ERR-FLG   =   ZERO
                     MOVE   1      TO   SYR-FLG
              END-IF
     WHEN     OTHER
              MOVE      1         TO   ERR-FLG
     END-EVALUATE.
*
 DSP-DPNO-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      存在チェック処理（伝票_）                  *
*--------------------------------------------------------------*
 CHK-DPNO-RTN           SECTION.
*全レコード解放（排他制御用）
     CLOSE    DNSFILF.
     OPEN     I-O   DNSFILF.
* ヘッド部属性クリア
     PERFORM   CLR-HEAD-RTN.
*
     IF       DPNO      IS  NOT  NUMERIC
              MOVE  0        TO  DPNO
     END-IF.
* 入出庫ファイル，存在チェック
     MOVE     DPNO           TO   DNS-F01.
     MOVE     0              TO   DNS-F02.
     START    DNSFILF   KEY  >=   DNS-F01  DNS-F02
         INVALID    KEY
                MOVE   1       TO   DNS-FLG
         NOT INVALID KEY
                PERFORM        DNS-READ-RTN
     END-START.
* 登録以外の時
     IF      (DNS-FLG         =   1)
             MOVE   2        TO   ERR-FLG
             MOVE   "C"      TO   EDIT-CURSOR  OF  DPNO
             MOVE   "R"      TO   EDIT-OPTION  OF  DPNO
             GO  TO   CHK-DPNO-EXIT
     END-IF.
* D365連携区分チェック
     IF      DNS-F22          =    "1"
             MOVE   16       TO   ERR-FLG
             MOVE   "C"      TO   EDIT-CURSOR  OF  DPNO
             MOVE   "R"      TO   EDIT-OPTION  OF  DPNO
             GO  TO   CHK-DPNO-EXIT
     END-IF.
* 該当データを画面にセットする
     MOVE    HEAD1           TO   SAV-HEAD1.
     MOVE    SPACE           TO   FVD04901.
     MOVE    SAV-HEAD1       TO   HEAD1.
     PERFORM  VARYING  I  FROM  1  BY  1  UNTIL  I   >  4
             MOVE   GYO-TBL(I)     TO  GYONO(I)
     END-PERFORM.
     PERFORM     DNS-DSP-RTN.
*
 CHK-DPNO-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      前レコードチェック処理                      *
*--------------------------------------------------------------*
 MAE-REC-RTN            SECTION.
*全レコード解放（排他制御用）
     CLOSE    DNSFILF.
     OPEN     I-O   DNSFILF.
* 属性クリア
     MOVE     ZERO      TO  DNS-FLG.
     PERFORM   CLR-HEAD-RTN.
*
     IF       DPNO      IS  NOT  NUMERIC
              MOVE  0        TO  DPNO
     END-IF.
* 伝票_存在チェック
     MOVE     DPNO           TO  DNS-F01.
     MOVE     0              TO  DNS-F02.
     START    DNSFILF   KEY  IS  <  DNS-F01  DNS-F02
                 WITH   REVERSED  ORDER
         INVALID  KEY
              MOVE  1        TO  DNS-FLG
         NOT  INVALID  KEY
              PERFORM  DNS-READ1-RTN
              IF  DNS-FLG           =  0
                  MOVE    DNS-F01     TO  WK-F01
                  MOVE    WK-F01      TO  DNS-F01
                  MOVE    0           TO  DNS-F02
                  START    DNSFILF   KEY  >=   DNS-F01  DNS-F02
                      INVALID    KEY
                           MOVE   1       TO   DNS-FLG
                      NOT INVALID KEY
                          PERFORM        DNS-READ1-RTN
                  END-START
              END-IF
     END-START.
     IF      DNS-FLG          =   1
             MOVE   14       TO   ERR-FLG
             MOVE   "C"      TO   EDIT-CURSOR  OF  DPNO
             MOVE   "R"      TO   EDIT-OPTION  OF  DPNO
             GO  TO          MAE-REC-EXIT
     END-IF.
* 該当データを画面にセット
     MOVE    HEAD1           TO   SAV-HEAD1.
     MOVE    SPACE           TO   FVD04901.
     MOVE    SAV-HEAD1       TO   HEAD1.
     PERFORM  VARYING   I  FROM  1  BY  1
                UNTIL   I     >  4
             MOVE  GYO-TBL(I)   TO  GYONO(I)
     END-PERFORM.
     PERFORM     DNS-DSP-RTN.
*
 MAE-REC-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      次レコードチェック処理                      *
*--------------------------------------------------------------*
 TUGI-REC-RTN           SECTION.
*全レコード解放（排他制御用）
     CLOSE    DNSFILF.
     OPEN     I-O   DNSFILF.
* 属性クリア
     MOVE     ZERO      TO  DNS-FLG.
     PERFORM   CLR-HEAD-RTN.
*
     IF       DPNO      IS  NOT  NUMERIC
              MOVE  0        TO  DPNO
     END-IF.
* 伝票_存在チェック
     MOVE     DPNO           TO  DNS-F01.
     MOVE     99             TO  DNS-F02.
     START    DNSFILF   KEY   >  DNS-F01  DNS-F02
         INVALID  KEY
              MOVE  1        TO  DNS-FLG
         NOT  INVALID  KEY
              PERFORM  DNS-READ1-RTN
     END-START.
     IF  DNS-FLG          =   1
         MOVE   13       TO   ERR-FLG
         MOVE   "C"      TO   EDIT-CURSOR  OF  DPNO
         MOVE   "R"      TO   EDIT-OPTION  OF  DPNO
         GO  TO          TUGI-REC-EXIT
     END-IF.
     MOVE    SPACE           TO   FVD04901.
     MOVE    SAV-HEAD1       TO   HEAD1.
     PERFORM  VARYING   I  FROM  1  BY  1
                UNTIL   I     >  4
             MOVE  GYO-TBL(I)   TO  GYONO(I)
     END-PERFORM.
     PERFORM     DNS-DSP-RTN.
*
 TUGI-REC-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3         画面へ表示　　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 DNS-DSP-RTN            SECTION.
*データ表示
     MOVE      SPACE         TO    TABLE-G.
     PERFORM   DNS-TO-HEAD.
     PERFORM   VARYING  I  FROM  1  BY  1  UNTIL    I  >  6
                                     OR       DNS-FLG  =  1
            IF    DPNO     =  DNS-F01
                  ADD   1    TO   MAX-LINE
**********●      IF    I        =  DNS-F02
                        PERFORM   DNS-TO-WRK
                        PERFORM   DNS-READ-RTN
**********●      END-IF
            END-IF
     END-PERFORM.
     PERFORM   VARYING  I  FROM  1  BY  1
               UNTIL    I  >  4
                  OR    I  >  MAX-LINE
            PERFORM   WRK-TO-BODY
     END-PERFORM.
* 入力行以降 プロテクトセット
     IF  I      <=     4
         PERFORM    UNTIL    I   >   4
               PERFORM   PRO-BODY-RTN
               ADD     1        TO   I
         END-PERFORM
     END-IF.
     IF  MAX-LINE >    4
         MOVE      2   TO  P-MAX
     ELSE
         MOVE      1   TO  P-MAX
     END-IF.
 DNS-DSP-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＧＢＯＤＹ１　入力　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 DSP-BODY-RTN     SECTION.
     PERFORM  PRO-SET-RTN.
*
     MOVE      G003          TO   GUIDE.
     MOVE     "BODY"         TO   WK-GRP.
     PERFORM  DSP-RD-RTN.
* アテンション判定
     EVALUATE DSP-FNC
     WHEN     "F004"
              MOVE      0         TO   SYR-FLG
     WHEN     "F005"
              MOVE      "END"     TO   END-FLG
     WHEN     "F006"
              MOVE      3         TO   SYR-FLG
              PERFORM  CLR-BODY-RTN
              MOVE   ZERO     TO   DNS-FLG
     WHEN     "F011"
              COMPUTE  C-CNT  =  P-CNT  -  1
              IF  C-CNT  =  ZERO
                  MOVE   22        TO   ERR-FLG

              ELSE
                  PERFORM  CLR-BODY-RTN
                  MOVE   ZERO         TO   GYO-CNT
                  PERFORM  CHK-BODY-RTN
                            VARYING  L  FROM 1  BY  1
                              UNTIL  L  >  4
                  IF   ERR-FLG  =  ZERO
                       PERFORM  DSP-TO-WRK
                            VARYING  I  FROM 1  BY  1
                              UNTIL  I  >  4
                       COMPUTE  P-CNT  =  P-CNT  -  1
                       PERFORM  GYO-SET-RTN
                       PERFORM  WRK-TO-BODY
                            VARYING  I  FROM 1  BY  1
                              UNTIL  I  >  4
                  END-IF
              END-IF
     WHEN     "F012"
              COMPUTE C-CNT  =  P-CNT  +  1
              IF  C-CNT  >  P-MAX
                  MOVE   21        TO   ERR-FLG
              ELSE
                  PERFORM  CLR-BODY-RTN
                  MOVE   ZERO         TO   GYO-CNT
                  PERFORM  CHK-BODY-RTN
                            VARYING  L  FROM 1  BY  1
                              UNTIL  L  >  4
                  IF   ERR-FLG  =  ZERO
                       PERFORM  DSP-TO-WRK
                            VARYING  I  FROM 1  BY  1
                              UNTIL  I  >  4
                       COMPUTE P-CNT  =  P-CNT  +  1
                       MOVE     SPACE      TO   BODY1
                       PERFORM  GYO-SET-RTN
                       PERFORM  WRK-TO-BODY
                            VARYING  I  FROM 1  BY  1
                              UNTIL  I  >  4
                       PERFORM  CLR-BODY-RTN
                  END-IF

              END-IF
     WHEN     "E000"
              PERFORM  CLR-BODY-RTN
              MOVE   ZERO         TO   GYO-CNT
              PERFORM  CHK-BODY-RTN
                            VARYING  L  FROM 1  BY  1
                              UNTIL  L  >  4
              IF   ERR-FLG  =  ZERO
                   PERFORM  DSP-TO-WRK
                            VARYING  I  FROM 1  BY  1
                              UNTIL  I  >  4
                   PERFORM  CLR-BODY-RTN
                   PERFORM   CLR-TAIL-RTN
                   MOVE      "Y"       TO   KAKNIN
                   MOVE      90        TO   SYR-FLG
*---<  商品在庫マスタ存在チェック  >---*
                   PERFORM  CLR-BODY-RTN
                   PERFORM  ZAI-CHK-SEC
              END-IF
     WHEN     OTHER
              MOVE      1         TO   ERR-FLG
     END-EVALUATE.
 DSP-BODY-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEBEL  3       ＢＯＤＹ　チェック　　　　 　　　　　　　*
*--------------------------------------------------------------*
 GYO-SET-RTN         SECTION.
     IF     P-CNT   =   1
            PERFORM  VARYING   I  FROM  1  BY  1
                       UNTIL   I     >  4
                     MOVE  GYO-TBL(I)   TO  GYONO(I)
            END-PERFORM
     ELSE
            PERFORM  VARYING   I  FROM  5  BY  1
                       UNTIL   I     >  8
                     MOVE  GYO-TBL(I)   TO  GYONO(I - 4)
            END-PERFORM
     END-IF.
*
 GYO-SET--EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEBEL  3       ＢＯＤＹ　チェック　　　　 　　　　　　　*
*--------------------------------------------------------------*
 CHK-BODY-RTN         SECTION.
     IF       SYOCD(L)             =   SPACE
              GO  TO  CHK-BODY-90
     END-IF.
* 明細件数のカウント
     ADD   1                    TO   GYO-CNT.
*
*商品名取得
     MOVE   SYOCD(L)         TO  MEI-F011.
     MOVE   HIN101(L)        TO  MEI-F0121.
     MOVE   HIN201(L)        TO  MEI-F0122.
     MOVE   HIN301(L)        TO  MEI-F0123.
     PERFORM   MEI-READ-RTN.
     IF   INV-FLG          =   1
          MOVE  SPACE        TO  HINMEI(L)
     ELSE
          MOVE  MEI-F021     TO  WK-HINMEI1
          MOVE  MEI-F022     TO  WK-HINMEI2
          MOVE  WK-HINMEI    TO  HINMEI(L)
     END-IF.
*数量チェック
     IF  JITUSU(L)     NOT  NUMERIC
         MOVE   ZERO   TO   JITUSU(L)
     END-IF.
     IF  JITUSU(L)        =   ZERO
*********2021/07/02 NAV ST
         CONTINUE
***      IF    ERR-FLG      =   ZERO
***            MOVE   6     TO  ERR-FLG
***      END-IF
***      MOVE  "C"          TO  EDIT-CURSOR  OF JITUSU(L)
***      MOVE  "R"          TO  EDIT-OPTION  OF JITUSU(L)
*********2021/07/02 NAV ED
     ELSE
         IF   SIJISU(L)  <  JITUSU(L)
              IF    ERR-FLG      =   ZERO
                    MOVE   25    TO  ERR-FLG
              END-IF
              MOVE  "C"          TO  EDIT-CURSOR  OF JITUSU(L)
              MOVE  "R"          TO  EDIT-OPTION  OF JITUSU(L)
         END-IF
     END-IF.
*#2021/04/14 NAV ST
     MOVE     ZERO               TO  WK-STNO-GK.
     IF  STK101(L)  =  SPACE
         MOVE        0           TO  STS101(L)
     END-IF.
     IF  STK201(L)  =  SPACE
         MOVE        0           TO  STS201(L)
     END-IF.
     IF  STK301(L)  =  SPACE
         MOVE        0           TO  STS301(L)
     END-IF.
     IF  STK401(L)  =  SPACE
         MOVE        0           TO  STS401(L)
     END-IF.
     IF  STK501(L)  =  SPACE
         MOVE        0           TO  STS501(L)
     END-IF.
     IF  STS101(L)  NOT NUMERIC
         ADD         0           TO  WK-STNO-GK
     ELSE
         ADD         STS101(L)   TO  WK-STNO-GK
     END-IF.
     IF  STS201(L)  NOT NUMERIC
         ADD         0           TO  WK-STNO-GK
     ELSE
         ADD         STS201(L)   TO  WK-STNO-GK
     END-IF.
     IF  STS301(L)  NOT NUMERIC
         ADD         0           TO  WK-STNO-GK
     ELSE
         ADD         STS301(L)   TO  WK-STNO-GK
     END-IF.
     IF  STS401(L)  NOT NUMERIC
         ADD         0           TO  WK-STNO-GK
     ELSE
         ADD         STS401(L)   TO  WK-STNO-GK
     END-IF.
     IF  STS501(L)  NOT NUMERIC
         ADD         0           TO  WK-STNO-GK
     ELSE
         ADD         STS501(L)   TO  WK-STNO-GK
     END-IF.
*
     IF  STK101(L) =  SPACE   AND   STK201(L) =  SPACE
     AND STK301(L) =  SPACE   AND   STK401(L) =  SPACE
     AND STK501(L) =  SPACE
         CONTINUE
     ELSE
*********DISPLAY "CHK1 = " JITUSU(L) " - " WK-STNO-GK  UPON CONS
         IF   JITUSU(L)  NOT =  WK-STNO-GK
              IF    ERR-FLG      =   ZERO
                    MOVE   24    TO  ERR-FLG
              END-IF
              MOVE  "C"          TO  EDIT-CURSOR  OF STK101(L)
              MOVE  "R"          TO  EDIT-OPTION  OF STK101(L)
         END-IF
     END-IF.
*#2021/04/14 NAV ED
*
 CHK-BODY-90.
 CHK-BODY-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      確認　入力　　　　　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 DSP-KAKNIN-RTN    SECTION.
     MOVE      G002            TO   GUIDE.
     MOVE     "KAK001"         TO   WK-GRP.
     PERFORM  DSP-RD-RTN.
* アテンション判定
     EVALUATE DSP-FNC
     WHEN     "F004"
              MOVE      0         TO   SYR-FLG
     WHEN     "F005"
              MOVE      "END"     TO   END-FLG
     WHEN     "F006"
              MOVE      SPACE     TO   KAKNIN
              MOVE      4         TO   SYR-FLG
              PERFORM   CLR-TAIL-RTN
              PERFORM   CLR-BODY-RTN
     WHEN     "E000"
              PERFORM   CHK-KAKNIN-RTN
              PERFORM   CHK-WRK-RTN
                            VARYING  L  FROM 1  BY  1
                              UNTIL  L  >  6
              IF   ERR-FLG  =  ZERO
                   PERFORM   DNS-UPDT-RTN
                   PERFORM   CLR-TAIL-RTN
                   PERFORM   CLR-BODY-RTN
                   MOVE      0         TO   SYR-FLG
              END-IF
     WHEN     OTHER
              MOVE      1         TO   ERR-FLG
     END-EVALUATE.
*
 DSP-KAKNIN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEBEL  3       ワーク　チェック　　　　　　 　　　　　　
*--------------------------------------------------------------*
 CHK-WRK-RTN         SECTION.
     IF       TBL-SYOCD-R(L)    =   SPACE
              GO  TO  CHK-WRK-90
     END-IF.
* 明細件数のカウント
     ADD   1                    TO   GYO-CNT.
*
*商品名取得
     MOVE   TBL-SYOCD-R(L)      TO  MEI-F011.
     MOVE   TBL-HIN1-R(L)       TO  MEI-F0121.
     MOVE   TBL-HIN2-R(L)       TO  MEI-F0122.
     MOVE   TBL-HIN3-R(L)       TO  MEI-F0123.
     PERFORM   MEI-READ-RTN.
     IF   INV-FLG          =   1
          MOVE   23     TO  ERR-FLG
     END-IF.
*数量チェック
*#2021/07/02 NAV ST
**** IF  TBL-SYKSU-R(L)     NOT  NUMERIC
*        MOVE   ZERO   TO   TBL-SYKSU-R(L)
*    END-IF.
*    IF  TBL-SYKSU-R(L)     =   ZERO
*        IF    ERR-FLG      =   ZERO
*              MOVE   23     TO  ERR-FLG
*        END-IF
**** END-IF.
*#2021/07/02 NAV ED
*
 CHK-WRK-90.
 CHK-WRK-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      確認　　　　チェック　　　　　　　　　　　　*
*--------------------------------------------------------------*
 CHK-KAKNIN-RTN     SECTION.
     MOVE     "M"        TO   EDIT-OPTION OF KAKNIN.
     IF      (KAKNIN  NOT  =  "Y") AND
             (KAKNIN  NOT  =  " ")
              MOVE      7         TO   ERR-FLG
              MOVE     "R"        TO   EDIT-OPTION OF KAKNIN
     END-IF.
*
 CHK-KAKNIN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      入出庫更新　処理　　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 DNS-UPDT-RTN          SECTION.
     MOVE     SYKYMD              TO   WK-NYS-DATE.
*
     PERFORM  DNS-REWRITE.
 DNS-UPDT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      確認後画面　処理　　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 KAKNIN-AFTER-RTN          SECTION.
*
     EVALUATE    KAKNIN
         WHEN    "Y"
                 MOVE   SPACE     TO   FVD04901
                 PERFORM   VARYING   I  FROM  1  BY  1
                             UNTIL   I     >  4
                    MOVE   GYO-TBL(I)     TO   GYONO(I)
                 END-PERFORM
                 MOVE   1         TO   SYR-FLG
*
     END-EVALUATE.
 KAKNIN-AFTER-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4     入出庫ファイル更新処理　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DNS-REWRITE            SECTION.
*入出庫ファイル更新
     MOVE   DPNO             TO   DNS-F01.
     MOVE   0                TO   DNS-F02.
     START    DNSFILF   KEY  >=   DNS-F01  DNS-F02
       INVALID    KEY
              MOVE   1       TO   DNS-FLG
       NOT INVALID KEY
              PERFORM        DNS-READ-RTN
     END-START.
     PERFORM  VARYING  I  FROM  1  BY  1
                UNTIL  I     >  6  OR  DNS-FLG  =  1
**********●  IF  I            =  DNS-F02
                  PERFORM    ZAI-OLD-RTN
                  PERFORM    WRK-TO-DNS
                  PERFORM    ZAI-NEW-RTN
                  REWRITE    DNS-REC
                  PERFORM    DNS-READ-RTN
**********●  END-IF
     END-PERFORM.
 DNS-REWRITE-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4     表示項目を入出庫ファイルに転送               *
*--------------------------------------------------------------*
 DSP-TO-WRK             SECTION.
* 画面データを入出庫ファイルの項目にセット
     MOVE   SYOCD (I)   TO   TBL-SYOCD (P-CNT I).
     MOVE   HIN101(I)   TO   TBL-HIN1  (P-CNT I).
     MOVE   HIN201(I)   TO   TBL-HIN2  (P-CNT I).
     MOVE   HIN301(I)   TO   TBL-HIN3  (P-CNT I).
     MOVE   SIJISU(I)   TO   TBL-SIJISU(P-CNT I).
     MOVE   TAN101(I)   TO   TBL-STAN1 (P-CNT I).
     MOVE   TAN201(I)   TO   TBL-STAN2 (P-CNT I).
     MOVE   TAN301(I)   TO   TBL-STAN3 (P-CNT I).
     MOVE   STK101(I)   TO   TBL-STK1  (P-CNT I).
     MOVE   STK201(I)   TO   TBL-STK2  (P-CNT I).
     MOVE   STK301(I)   TO   TBL-STK3  (P-CNT I).
     MOVE   STK401(I)   TO   TBL-STK4  (P-CNT I).
     MOVE   STK501(I)   TO   TBL-STK5  (P-CNT I).
     IF     STS101(I)   NUMERIC
            MOVE   STS101(I)   TO   TBL-STS1  (P-CNT I)
     ELSE
            MOVE   ZERO        TO   TBL-STS1  (P-CNT I)
     END-IF.
     IF     STS201(I)   NUMERIC
            MOVE   STS201(I)   TO   TBL-STS2  (P-CNT I)
     ELSE
            MOVE   ZERO        TO   TBL-STS2  (P-CNT I)
     END-IF.
     IF     STS301(I)   NUMERIC
            MOVE   STS301(I)   TO   TBL-STS3  (P-CNT I)
     ELSE
            MOVE   ZERO        TO   TBL-STS3  (P-CNT I)
     END-IF.
     IF     STS401(I)   NUMERIC
            MOVE   STS401(I)   TO   TBL-STS4  (P-CNT I)
     ELSE
            MOVE   ZERO        TO   TBL-STS4  (P-CNT I)
     END-IF.
     IF     STS501(I)   NUMERIC
            MOVE   STS501(I)   TO   TBL-STS5  (P-CNT I)
     ELSE
            MOVE   ZERO        TO   TBL-STS5  (P-CNT I)
     END-IF.
     MOVE   JITUSU(I)   TO   TBL-SYKSU (P-CNT I).
     MOVE   BIKOU1(I)   TO   TBL-BIKOU (P-CNT I).
 DSP-TO-WRK-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4     ワークを入出庫ファイルに転送                 *
*--------------------------------------------------------------*
 WRK-TO-DNS             SECTION.
* 画面データを入出庫ファイルの項目にセット
     MOVE   TBL-STAN1-R (I)       TO   DNS-F07(1:1).
     MOVE   TBL-STAN2-R (I)       TO   DNS-F07(2:3).
     MOVE   TBL-STAN3-R (I)       TO   DNS-F07(5:2).
*
     MOVE   TBL-STK1-R (I)        TO   DNS-F25(1).
     MOVE   TBL-STK2-R (I)        TO   DNS-F25(2).
     MOVE   TBL-STK3-R (I)        TO   DNS-F25(3).
     MOVE   TBL-STK4-R (I)        TO   DNS-F25(4).
     MOVE   TBL-STK5-R (I)        TO   DNS-F25(5).
     MOVE   TBL-STS1-R (I)        TO   DNS-F28(1).
     MOVE   TBL-STS2-R (I)        TO   DNS-F28(2).
     MOVE   TBL-STS3-R (I)        TO   DNS-F28(3).
     MOVE   TBL-STS4-R (I)        TO   DNS-F28(4).
     MOVE   TBL-STS5-R (I)        TO   DNS-F28(5).
     MOVE   WK-NYS-DATE           TO   DNS-F14.
     MOVE   TBL-SYKSU-R(I)        TO   DNS-F15.
     MOVE   TANTO                 TO   DNS-F16.
     MOVE   "1"                   TO   DNS-F17.
     MOVE   TBL-BIKOU-R(I)        TO   DNS-F27.
     MOVE   SYS-DATE2             TO   DNS-F97.
     MOVE   SYS-TIME              TO   DNS-F98.
     MOVE   TANTO                 TO   DNS-F99.
 WRK-TO-DNS-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4     ヘッド部読込　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DNS-TO-HEAD            SECTION.
* ヘッド部のデータを画面にセット
     MOVE   DNS-F01          TO   DPNO.
     MOVE   SYS-DATE2        TO   SYKYMD.
     MOVE   LINK-TANCD       TO   TANTO.
     MOVE   SPACE            TO   TANNM.
     MOVE    TANTO           TO   TAN-F02.
     PERFORM TAN-READ-RTN.
     IF   INV-FLG     =  ZERO
          MOVE   TAN-F03  TO  TANNM
     END-IF.
* 倉庫場所のセット
     MOVE  DNS-F05    TO   BASY01  SOK-F01.
     PERFORM   SOK-READ-RTN.
     IF   INV-FLG           =   1
         MOVE    NC"＊＊＊＊＊＊＊＊＊＊"        TO  BSYM1
     ELSE
         MOVE    SOK-F02  TO   BSYM1
         EVALUATE SOK-F14
             WHEN SPACE
                  MOVE  NC"ＮＡＶＳ倉庫"         TO  BUNM1
             WHEN "1"
                  MOVE  NC"ＳＬＩＭＳ倉庫"       TO  BUNM1
             WHEN OTHER
                  MOVE  NC"＊＊＊＊＊＊＊＊＊＊" TO  BUNM1
         END-EVALUATE
     END-IF.
     MOVE  DNS-F08    TO   BASY02  SOK-F01.
     PERFORM   SOK-READ-RTN.
     IF  INV-FLG           =   1
         MOVE    NC"＊＊＊＊＊＊＊＊＊＊"       TO  BSYM2
     ELSE
        MOVE    SOK-F02  TO   BSYM2
        EVALUATE SOK-F14
            WHEN SPACE
                 MOVE  NC"ＮＡＶＳ倉庫"         TO  BUNM2
            WHEN "1"
                 MOVE  NC"ＳＬＩＭＳ倉庫"       TO  BUNM2
            WHEN OTHER
                 MOVE  NC"＊＊＊＊＊＊＊＊＊＊" TO  BUNM2
        END-EVALUATE
     END-IF.
 DNS-TO-HEAD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4     ワーク読込　　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DNS-TO-WRK             SECTION.
* ボディ部データセット
     MOVE   DNS-F02               TO   TBL-GYONO-R (I).
     MOVE   DNS-F03               TO   TBL-SYOCD-R (I).
     MOVE   DNS-F04(1:5)          TO   TBL-HIN1-R  (I).
     MOVE   DNS-F04(6:2)          TO   TBL-HIN2-R  (I).
     MOVE   DNS-F04(8:1)          TO   TBL-HIN3-R  (I).
     MOVE   DNS-F13               TO   TBL-SIJISU-R(I).
     MOVE   DNS-F07(1:1)          TO   TBL-STAN1-R (I).
     MOVE   DNS-F07(2:3)          TO   TBL-STAN2-R (I).
     MOVE   DNS-F07(5:2)          TO   TBL-STAN3-R (I).
     MOVE   DNS-F25(1)            TO   TBL-STK1-R  (I).
     MOVE   DNS-F25(2)            TO   TBL-STK2-R  (I).
     MOVE   DNS-F25(3)            TO   TBL-STK3-R  (I).
     MOVE   DNS-F25(4)            TO   TBL-STK4-R  (I).
     MOVE   DNS-F25(5)            TO   TBL-STK5-R  (I).
     MOVE   DNS-F28(1)            TO   TBL-STS1-R  (I).
     MOVE   DNS-F28(2)            TO   TBL-STS2-R  (I).
     MOVE   DNS-F28(3)            TO   TBL-STS3-R  (I).
     MOVE   DNS-F28(4)            TO   TBL-STS4-R  (I).
     MOVE   DNS-F28(5)            TO   TBL-STS5-R  (I).
     IF     DNS-F15    =    ZERO
            MOVE   DNS-F13        TO   TBL-SYKSU-R (I)
     ELSE
            MOVE   DNS-F15        TO   TBL-SYKSU-R (I)
     END-IF.
     MOVE   DNS-F27               TO   TBL-BIKOU-R (I).
 DNS-TO-WRK-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4     ボディ部編集　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 WRK-TO-BODY            SECTION.
* ボディ部データセット
     IF     TBL-SYOCD (P-CNT I)   NOT =  SPACE
            MOVE   TBL-SYOCD (P-CNT I)   TO   SYOCD (I)
            MOVE   TBL-HIN1  (P-CNT I)   TO   HIN101(I)
            MOVE   TBL-HIN2  (P-CNT I)   TO   HIN201(I)
            MOVE   TBL-HIN3  (P-CNT I)   TO   HIN301(I)
            MOVE   TBL-SIJISU(P-CNT I)   TO   SIJISU(I)
            MOVE   TBL-STAN1 (P-CNT I)   TO   TAN101(I)
            MOVE   TBL-STAN2 (P-CNT I)   TO   TAN201(I)
            MOVE   TBL-STAN3 (P-CNT I)   TO   TAN301(I)
            MOVE   TBL-STK1  (P-CNT I)   TO   STK101(I)
            MOVE   TBL-STK2  (P-CNT I)   TO   STK201(I)
            MOVE   TBL-STK3  (P-CNT I)   TO   STK301(I)
            MOVE   TBL-STK4  (P-CNT I)   TO   STK401(I)
            MOVE   TBL-STK5  (P-CNT I)   TO   STK501(I)
            IF  TBL-STS1  (P-CNT I)      NOT =    ZERO
                MOVE   TBL-STS1  (P-CNT I)   TO   STS101(I)
            END-IF
            IF  TBL-STS2  (P-CNT I)      NOT =    ZERO
                MOVE   TBL-STS2  (P-CNT I)   TO   STS201(I)
            END-IF
            IF  TBL-STS3  (P-CNT I)      NOT =    ZERO
                MOVE   TBL-STS3  (P-CNT I)   TO   STS301(I)
            END-IF
            IF  TBL-STS4  (P-CNT I)      NOT =    ZERO
                MOVE   TBL-STS4  (P-CNT I)   TO   STS401(I)
            END-IF
            IF  TBL-STS5  (P-CNT I)      NOT =    ZERO
                MOVE   TBL-STS5  (P-CNT I)   TO   STS501(I)
            END-IF
            IF  TBL-SYKSU (P-CNT I)      NOT =    ZERO
                MOVE   TBL-SYKSU (P-CNT I)   TO   JITUSU(I)
            END-IF
            MOVE   TBL-BIKOU (P-CNT I)   TO   BIKOU1(I)
* 商品名称取得
            MOVE   TBL-SYOCD (P-CNT I)   TO  MEI-F011
            MOVE   TBL-HIN1  (P-CNT I)   TO  MEI-F0121
            MOVE   TBL-HIN2  (P-CNT I)   TO  MEI-F0122
            MOVE   TBL-HIN3  (P-CNT I)   TO  MEI-F0123
            PERFORM   MEI-READ-RTN
            IF  INV-FLG                   =   1
                MOVE  SPACE              TO  HINMEI(I)
            ELSE
                MOVE  MEI-F021           TO  WK-HINMEI1
                MOVE  MEI-F022           TO  WK-HINMEI2
                MOVE  WK-HINMEI          TO  HINMEI(I)
            END-IF
     ELSE
            MOVE    SPACE                TO  MEISAI(I)
            PERFORM   PRO-BODY-RTN
     END-IF.
 WRK-TO-BODY-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL     ﾃﾞｨｽﾌﾟﾚｰ  READ                              *
*--------------------------------------------------------------*
 DSP-RD-RTN           SECTION.
*
     IF       ERR-FLG  =  0
              MOVE      SPACE               TO   MESAGE
     ELSE
              MOVE      MSG-TBL (ERR-FLG)   TO   MESAGE
     END-IF.
     MOVE     "ALLF"         TO   DSP-GRP.
     PERFORM  DSP-WRITE-RTN.
*
     IF       ERR-FLG  NOT  =  0
              MOVE      "AL"           TO   DSP-PRO
              MOVE      0              TO   ERR-FLG
     ELSE
              MOVE      "NE"           TO   DSP-PRO
     END-IF.
*
     MOVE     WK-GRP         TO   DSP-GRP.
     READ     DSPFILE.
     MOVE     SPACE          TO   DSP-PRO.
 DSP-RD-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL     ﾃﾞｨｽﾌﾟﾚｰ  WRITE                             *
*--------------------------------------------------------------*
 DSP-WRITE-RTN          SECTION.
     MOVE     HEN-DATE       TO   SDATE.
     MOVE     HEN-TIME       TO   STIME.
*
     MOVE     SPACE          TO   DSP-PRO.
     WRITE    FVD04901.
 DSP-WRITE-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  ALL   入出庫ファイル ＲＥＡＤ
*--------------------------------------------------------------*
 DNS-READ-RTN       SECTION.
     MOVE     ZERO      TO    DNS-FLG.
     READ     DNSFILF   NEXT  AT  END
              MOVE      1         TO   DNS-FLG
              GO   TO   DNS-READ-EXIT
     END-READ.
* 伝票_ブレイク
     IF   DPNO     NOT =   DNS-F01
          MOVE  1     TO   DNS-FLG
          GO    TO    DNS-READ-EXIT
     END-IF.
 DNS-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  ALL   入出庫ファイル ＲＥＡＤ１
*--------------------------------------------------------------*
 DNS-READ1-RTN       SECTION.
     READ     DNSFILF   NEXT  AT  END
              MOVE      1         TO   DNS-FLG
              GO   TO   DNS-READ1-EXIT
     END-READ.
 DNS-READ1-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  ALL   商品在庫マスタ（変更前情報の更新）　　　　　 *
*--------------------------------------------------------------*
 ZAI-OLD-RTN        SECTION.
*出庫情報
     MOVE     2                 TO   ZAI-FLG.
     MOVE     DNS-F05           TO   ZAI-F01.
     MOVE     DNS-F03           TO   ZAI-F021.
     MOVE     DNS-F04           TO   ZAI-F022.
     MOVE     DNS-F07           TO   ZAI-F03.
     PERFORM    ZAI-READ-RTN.
     IF    INV-FLG    =  1
           PERFORM   ZAI-INIT-RTN
           PERFORM   ZAI-SUBTRACT-RTN
           MOVE      SYS-DATE2    TO   ZAI-F98
           WRITE     ZAI-REC
     ELSE
           PERFORM   ZAI-SUBTRACT-RTN
           MOVE      SYS-DATE2    TO   ZAI-F99
           REWRITE   ZAI-REC
     END-IF.
*入庫情報
     MOVE     1                 TO   ZAI-FLG.
     MOVE     DNS-F08           TO   ZAI-F01.
     MOVE     DNS-F03           TO   ZAI-F021.
     MOVE     DNS-F04           TO   ZAI-F022.
     MOVE     DNS-F10           TO   ZAI-F03.
     PERFORM    ZAI-READ-RTN.
     IF    INV-FLG    =  1
           PERFORM   ZAI-INIT-RTN
           PERFORM   ZAI-SUBTRACT-RTN
           MOVE      SYS-DATE2    TO   ZAI-F98
********** WRITE     ZAI-REC
     ELSE
           PERFORM   ZAI-SUBTRACT-RTN
           MOVE      SYS-DATE2    TO   ZAI-F99
********** REWRITE   ZAI-REC
     END-IF.
 ZAI-OLD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  ALL   商品在庫マスタ（変更後情報の更新）　　　　　 *
*--------------------------------------------------------------*
 ZAI-NEW-RTN        SECTION.
*出庫情報
     MOVE     2                 TO   ZAI-FLG.
     MOVE     DNS-F05           TO   ZAI-F01.
     MOVE     DNS-F03           TO   ZAI-F021.
     MOVE     DNS-F04           TO   ZAI-F022.
     MOVE     DNS-F07           TO   ZAI-F03.
     PERFORM    ZAI-READ-RTN.
     IF    INV-FLG    =  1
           PERFORM   ZAI-INIT-RTN
           PERFORM   ZAI-ADD-RTN
           MOVE      SYS-DATE2    TO   ZAI-F98
           WRITE     ZAI-REC
     ELSE
           PERFORM   ZAI-ADD-RTN
           MOVE      SYS-DATE2    TO   ZAI-F99
           REWRITE   ZAI-REC
     END-IF.
*入庫情報
     MOVE  1                 TO   ZAI-FLG.
     MOVE  DNS-F08           TO   ZAI-F01.
     MOVE  DNS-F03           TO   ZAI-F021.
     MOVE  DNS-F04           TO   ZAI-F022.
     MOVE  DNS-F10           TO   ZAI-F03.
     PERFORM    ZAI-READ-RTN.
     IF    INV-FLG    =  1
           PERFORM   ZAI-INIT-RTN
           PERFORM   ZAI-ADD-RTN
           MOVE      SYS-DATE2    TO   ZAI-F98
***********WRITE     ZAI-REC
     ELSE
           PERFORM   ZAI-ADD-RTN
           MOVE      SYS-DATE2    TO   ZAI-F99
***********REWRITE   ZAI-REC
     END-IF.
 ZAI-NEW-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  ALL   商品在庫マスタ　初期項目編集　　　　　　　　 *
*--------------------------------------------------------------*
 ZAI-INIT-RTN       SECTION.
     MOVE  SPACE              TO   ZAI-REC.
     INITIALIZE                    ZAI-REC.
*
     IF    ZAI-FLG     =    1
           MOVE  DNS-F08           TO   ZAI-F01
           MOVE  DNS-F03           TO   ZAI-F021
           MOVE  DNS-F04           TO   ZAI-F022
           MOVE  DNS-F10           TO   ZAI-F03
     ELSE
           MOVE  DNS-F05           TO   ZAI-F01
           MOVE  DNS-F03           TO   ZAI-F021
           MOVE  DNS-F04           TO   ZAI-F022
           MOVE  DNS-F07           TO   ZAI-F03
     END-IF.
*---<  商品名称（カナ）取得  >---*
     MOVE     ZAI-F021        TO      MEI-F011.
     MOVE     ZAI-F022        TO      MEI-F012.
     PERFORM  MEI-READ-RTN.
     MOVE     MEI-F031        TO      ZAI-F30.
*
 ZAI-INIT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  ALL   商品在庫・数量セット（変更前情報）
*--------------------------------------------------------------*
 ZAI-SUBTRACT-RTN   SECTION.
*
     IF    ZAI-FLG     =    2
*出庫情報
           COMPUTE  ZAI-F04  =  ZAI-F04  +  DNS-F15
           IF  DNS-F14  >  ZAI-SIME
                 COMPUTE  ZAI-F12  =  ZAI-F12  -  DNS-F15
           ELSE
                 COMPUTE  ZAI-F06  =  ZAI-F06  +  DNS-F15
                 COMPUTE  ZAI-F08  =  ZAI-F08  -  DNS-F15
           END-IF
     ELSE
*入庫情報
           COMPUTE  ZAI-F04  =  ZAI-F04  -  DNS-F15
           IF  DNS-F14  >  ZAI-SIME
                 COMPUTE  ZAI-F11  =  ZAI-F11  -  DNS-F15
           ELSE
                 COMPUTE  ZAI-F06  =  ZAI-F06  -  DNS-F15
                 COMPUTE  ZAI-F07  =  ZAI-F07  -  DNS-F15
           END-IF
     END-IF.
 ZAI-SUBTRACT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  ALL   商品在庫・数量セット（変更後情報）
*--------------------------------------------------------------*
 ZAI-ADD-RTN        SECTION.
*
     IF    ZAI-FLG     =    2
*出庫情報
           COMPUTE  ZAI-F04  =  ZAI-F04  -  DNS-F15
           IF  DNS-F14  >  ZAI-SIME
                 COMPUTE  ZAI-F12  =  ZAI-F12  +  DNS-F15
           ELSE
                 COMPUTE  ZAI-F06  =  ZAI-F06  -  DNS-F15
                 COMPUTE  ZAI-F08  =  ZAI-F08  +  DNS-F15
           END-IF
     ELSE
*入庫情報
           COMPUTE  ZAI-F04  =  ZAI-F04  +  DNS-F15
           IF  DNS-F14  >  ZAI-SIME
                 COMPUTE  ZAI-F11  =  ZAI-F11  +  DNS-F15
           ELSE
                 COMPUTE  ZAI-F06  =  ZAI-F06  +  DNS-F15
                 COMPUTE  ZAI-F07  =  ZAI-F07  +  DNS-F15
           END-IF
     END-IF.
 ZAI-ADD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  ALL   商品在庫マスタ　存在チェック　　　　　　　　 *
*--------------------------------------------------------------*
 ZAI-CHK-SEC        SECTION.
     PERFORM  VARYING  IDX1  FROM  1  BY  1   UNTIL  IDX1  >  4
         IF   SYOCD(IDX1)  NOT  =  SPACE
*
               MOVE  BASY01        TO   ZAI-F01
               MOVE  SYOCD (IDX1)  TO   ZAI-F021
               MOVE  HIN101(IDX1)  TO   ZAI-F022(1:5)
               MOVE  HIN201(IDX1)  TO   ZAI-F022(6:2)
               MOVE  HIN301(IDX1)  TO   ZAI-F022(8:1)
               MOVE  TAN101(IDX1)  TO   ZAI-F03 (1:1)
               MOVE  TAN201(IDX1)  TO   ZAI-F03 (2:3)
               MOVE  TAN301(IDX1)  TO   ZAI-F03 (5:2)
               PERFORM   ZAI-READ-RTN
               IF   INV-FLG   =  1
                    MOVE   "R"  TO   EDIT-OPTION  OF  SYOCD (IDX1)
                    MOVE   "R"  TO   EDIT-OPTION  OF  HIN101(IDX1)
                    MOVE   "R"  TO   EDIT-OPTION  OF  HIN201(IDX1)
                    MOVE   "R"  TO   EDIT-OPTION  OF  HIN301(IDX1)
                    MOVE   "R"  TO   EDIT-OPTION  OF  TAN101(IDX1)
                    MOVE   "R"  TO   EDIT-OPTION  OF  TAN201(IDX1)
                    MOVE   "R"  TO   EDIT-OPTION  OF  TAN301(IDX1)
                    MOVE   18   TO   ERR-FLG
               ELSE
                    REWRITE   ZAI-REC
               END-IF
*
         END-IF
     END-PERFORM.
 ZAI-CHK-END.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  ALL   商品在庫マスタ　ＲＥＡＤ　　　　　　　　　　 *
*--------------------------------------------------------------*
 ZAI-READ-RTN       SECTION.
     MOVE     0         TO   INV-FLG.
     READ     ZAMZAIF   INVALID   KEY
              MOVE      1         TO   INV-FLG
     END-READ.
 ZAI-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  ALL   条件ファイル　　　ＲＥＡＤ　　　　　　　　　 *
*--------------------------------------------------------------*
 JYO-RD-RTN         SECTION.
     MOVE     0         TO   INV-FLG.
     READ     HJYOKEN   INVALID   KEY
              MOVE      1         TO   INV-FLG
     END-READ.
 JYO-RD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  ALL   商品名称マスタ　　ＲＥＡＤ　　　　　　　　　 *
*--------------------------------------------------------------*
 MEI-READ-RTN       SECTION.
     MOVE     0         TO   INV-FLG.
     READ     HMEIMS    INVALID   KEY
              MOVE      1         TO   INV-FLG
     END-READ.
 MEI-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  ALL   倉庫マスタ　　　　ＲＥＡＤ　　　　　　　　　 *
*--------------------------------------------------------------*
 SOK-READ-RTN         SECTION.
     MOVE     0         TO   INV-FLG.
     READ     ZSOKMS    INVALID   KEY
              MOVE      1         TO   INV-FLG
     END-READ.
 SOK-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  ALL   担当者マスタ　　　ＲＥＡＤ　　　　　　　　　 *
*--------------------------------------------------------------*
 TAN-READ-RTN         SECTION.
     MOVE    0           TO   INV-FLG.
     MOVE    LINK-BUMON  TO   TAN-F01.
     READ    HTANMS      INVALID   KEY
             MOVE        1         TO   INV-FLG
     END-READ.
 TAN-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＨＥＡＤ　属性クリア　　　　　　　　　　　　*
*--------------------------------------------------------------*
 CLR-HEAD-RTN           SECTION.
     MOVE     " "            TO   EDIT-CURSOR OF TANTO
                                  EDIT-CURSOR OF DPNO
                                  EDIT-CURSOR OF SYKYMD.
     MOVE     "M"            TO   EDIT-OPTION OF TANTO
                                  EDIT-OPTION OF DPNO
                                  EDIT-OPTION OF SYKYMD.
*
 CLR-HEAD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＢＯＤＹ　属性クリア　　　　　　　　　　　　*
*--------------------------------------------------------------*
 CLR-BODY-RTN           SECTION.
     PERFORM  VARYING  I  FROM  1  BY  1  UNTIL  I  >  4
       MOVE     " "      TO   EDIT-CURSOR OF SYOCD (I)
                              EDIT-CURSOR OF HIN101(I)
                              EDIT-CURSOR OF HIN201(I)
                              EDIT-CURSOR OF HIN301(I)
       MOVE     " "      TO   EDIT-CURSOR OF TAN101(I)
                              EDIT-CURSOR OF TAN201(I)
                              EDIT-CURSOR OF TAN301(I)
                              EDIT-CURSOR OF JITUSU(I)
                              EDIT-CURSOR OF BIKOU1(I)
                              EDIT-CURSOR OF STK101(I)
                              EDIT-CURSOR OF STK201(I)
                              EDIT-CURSOR OF STK301(I)
                              EDIT-CURSOR OF STK401(I)
                              EDIT-CURSOR OF STK501(I)
       MOVE     "M"      TO   EDIT-OPTION OF SYOCD (I)
                              EDIT-OPTION OF HIN101(I)
                              EDIT-OPTION OF HIN201(I)
                              EDIT-OPTION OF HIN301(I)
       MOVE     "M"      TO   EDIT-OPTION OF TAN101(I)
                              EDIT-OPTION OF TAN201(I)
                              EDIT-OPTION OF TAN301(I)
                              EDIT-OPTION OF JITUSU(I)
                              EDIT-OPTION OF BIKOU1(I)
                              EDIT-OPTION OF STK101(I)
                              EDIT-OPTION OF STK201(I)
                              EDIT-OPTION OF STK301(I)
                              EDIT-OPTION OF STK401(I)
                              EDIT-OPTION OF STK501(I)
     END-PERFORM.
 CLR-BODY-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＴＡＩＬ　属性クリア　　　　　　　　　　　　*
*--------------------------------------------------------------*
 CLR-TAIL-RTN           SECTION.
     MOVE     " "        TO   EDIT-CURSOR OF KAKNIN.
     MOVE     "M"        TO   EDIT-OPTION OF KAKNIN.
 CLR-TAIL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      プロテクトセット（明細行）　　　　　　　　　*
*--------------------------------------------------------------*
 PRO-BODY-RTN           SECTION.
*未入力行にプロテクトセット
     MOVE  "X"           TO   EDIT-STATUS  OF  TAN101(I)
                              EDIT-STATUS  OF  TAN201(I)
                              EDIT-STATUS  OF  TAN301(I)
                              EDIT-STATUS  OF  JITUSU(I)
                              EDIT-STATUS  OF  BIKOU1(I)
                              EDIT-STATUS  OF  STK101(I)
                              EDIT-STATUS  OF  STK201(I)
                              EDIT-STATUS  OF  STK301(I)
                              EDIT-STATUS  OF  STK401(I)
                              EDIT-STATUS  OF  STK501(I)
                              EDIT-STATUS  OF  STS101(I)
                              EDIT-STATUS  OF  STS201(I)
                              EDIT-STATUS  OF  STS301(I)
                              EDIT-STATUS  OF  STS401(I)
                              EDIT-STATUS  OF  STS501(I).
 PRO-BODY-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      プロテクトセット（明細項目）　　　　　　　　*
*--------------------------------------------------------------*
 PRO-SET-RTN            SECTION.
     PERFORM  VARYING  I  FROM  1  BY  1  UNTIL  I  >  6
         IF   SYOCD(I)  NOT  =  SPACE
             MOVE  " "           TO   EDIT-STATUS  OF  TAN101(I)
                                      EDIT-STATUS  OF  TAN201(I)
                                      EDIT-STATUS  OF  TAN301(I)
                                      EDIT-STATUS  OF  JITUSU(I)
                                      EDIT-STATUS  OF  BIKOU1(I)
                                      EDIT-STATUS  OF  STK101(I)
                                      EDIT-STATUS  OF  STK201(I)
                                      EDIT-STATUS  OF  STK301(I)
                                      EDIT-STATUS  OF  STK401(I)
                                      EDIT-STATUS  OF  STK501(I)
                                      EDIT-STATUS  OF  STS101(I)
                                      EDIT-STATUS  OF  STS201(I)
                                      EDIT-STATUS  OF  STS301(I)
                                      EDIT-STATUS  OF  STS401(I)
                                      EDIT-STATUS  OF  STS501(I)
         END-IF
     END-PERFORM.
 PRO-SET-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*
