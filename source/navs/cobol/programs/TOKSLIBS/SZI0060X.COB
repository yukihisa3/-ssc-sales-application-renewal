****************************************************************
*    　　　　　　　　：　　　　　　　　　　　　　　　　　　　　*
*    　　　　　　　　：　　　　　　　　　　　　　　　　　　　　*
*    モジュール名　　：　入出庫入力(SZA0085I)
*    　　　　　　　　　　　　　　　　　　　　　　　　　　　　　*
*    　　　　　　　　：　在庫ＥＸＣＥＬデータ更新処理　        *
*    　　　　　　　　：　（入出庫系）の流用元　　　　　　　　  *
*    　　　　　　　　：　　　　　　　　　　　　　　　　　　　  *
*    　　　　　　　　：　　　　　　　　　　　　　　　　　　    *
*    　　　　　　　　：　　　　　　　　　　　　　　　　　　　　*
*    　　　　　　　　：　　　　　　　　　　　                  *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SZI0060X.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2016/06/16.
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       PRIMERGY6000.
 OBJECT-COMPUTER.       PRIMERGY6000.
 SPECIAL-NAMES.
     CONSOLE     IS     CONS
     STATION     IS     STAT.
***************************************************************
 INPUT-OUTPUT           SECTION.
***************************************************************
 FILE-CONTROL.
*----<< 入出庫ファイル >>-*
     SELECT   NYSFILF   ASSIGN    TO        DA-01-VI-NYSFILL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      DYNAMIC
                        RECORD    KEY       NYU-F02  NYU-F03
                        FILE      STATUS    NYU-ST1.
*----<< 在庫マスタ >>-*
     SELECT   ZAMZAIF   ASSIGN    TO        DA-01-VI-ZAMZAIL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       ZAI-F01  ZAI-F02
                                            ZAI-F03
                        FILE      STATUS    ZAI-ST1.
*----<< 条件ファイル >>-*
     SELECT   HJYOKEN   ASSIGN    TO        DA-01-VI-JYOKEN1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       JYO-F01   JYO-F02
                        FILE      STATUS    JYO-ST1.
*----<< 商品名称マスタ >>-*
     SELECT   HMEIMS    ASSIGN    TO        DA-01-VI-MEIMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      DYNAMIC
                        RECORD    KEY       MEI-F011   MEI-F0121
                                            MEI-F0122  MEI-F0123
                        FILE      STATUS    MEI-ST1.
*----<< 倉庫マスタ >>-*
*    SELECT   ZSOKMS    ASSIGN    TO        DA-01-VI-ZSOKMS1
*                       ORGANIZATION        INDEXED
*                       ACCESS    MODE      RANDOM
*                       RECORD    KEY       SOK-F01
*                       FILE      STATUS    SOK-ST1.
*----<< 作業区分マスタ >>-*
*    SELECT   SGYKBMF   ASSIGN    TO        DA-01-VI-SGYKBML1
*                       ORGANIZATION        INDEXED
*                       ACCESS    MODE      RANDOM
*                       RECORD    KEY       KBM-F01
*                       FILE      STATUS    KBM-ST1.
*----<< 画面ファイル >>-*
*    SELECT   DSPFILE   ASSIGN    TO        GS-DSPF
*                       SYMBOLIC  DESTINATION        "DSP"
*                       FORMAT              DSP-FMT
*                       GROUP               DSP-GRP
*                       PROCESSING  MODE    DSP-PRO
*                       SELECTED  FUNCTION  DSP-FNC
*                       FILE      STATUS    DSP-ST1.
******************************************************************
 DATA                      DIVISION.
******************************************************************
 FILE                      SECTION.
*----<< 入出庫ファイル >>-*
 FD  NYSFILF            LABEL     RECORD     IS  STANDARD.
     COPY     NYSFILF   OF   XFDLIB    JOINING   NYU  AS PREFIX.
*----<< 商品在庫マスタ >>-*
 FD  ZAMZAIF            LABEL     RECORD     IS  STANDARD.
     COPY     ZAMZAIF   OF   XFDLIB    JOINING   ZAI  AS PREFIX.
*----<< 条件ファイル >>-*
 FD  HJYOKEN            LABEL     RECORD     IS  STANDARD.
     COPY     JYOKEN1   OF   XFDLIB    JOINING   JYO  AS PREFIX.
*----<< 商品名称マスタ >>-*
 FD  HMEIMS             LABEL     RECORD     IS  STANDARD.
     COPY     MEIMS1    OF   XFDLIB    JOINING   MEI  AS PREFIX.
*----<< 倉庫マスタ >>-*
*FD  ZSOKMS             LABEL     RECORD     IS  STANDARD.
*    COPY     ZSOKMS1   OF   XFDLIB    JOINING   SOK  AS PREFIX.
*----<< 作業区分マスタ >>-*
*FD  SGYKBMF            LABEL     RECORD     IS  STANDARD.
*    COPY     SGYKBMF   OF   XFDLIB    JOINING   KBM  AS PREFIX.
*----<< 画面ファイル >>-*
*FD  DSPFILE.
*    COPY     FZA00851  OF   XMDLIB.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  PGM-ID                  PIC  X(08)     VALUE  "SZI0060B".
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
*01  DSP-CNTL.
*    03  DSP-FMT             PIC  X(08).
*    03  DSP-GRP             PIC  X(08).
*    03  DSP-PRO             PIC  X(02).
*    03  DSP-FNC             PIC  X(04).
*    03  DSP-ST1             PIC  X(02).
*    03  DSP-ST2             PIC  X(04).
*    03  DSP-CON             PIC  X(06).
*    03  WK-GRP.
*        05  WK-BODY             PIC  X(04).
*        05  WK-LINE             PIC  9(02).
*        05  FILLER              PIC  X(02).
 01  STATUS-AREA.
     03  IN-DATA             PIC  X(01).
     03  NYU-STATUS.
         05  NYU-ST1         PIC  X(02).
         05  NYU-ST2         PIC  X(04).
     03  ZAI-STATUS.
         05  ZAI-ST1         PIC  X(02).
         05  ZAI-ST2         PIC  X(04).
     03  JYO-STATUS.
         05  JYO-ST1         PIC  X(02).
         05  JYO-ST2         PIC  X(04).
     03  MEI-STATUS.
         05  MEI-ST1         PIC  X(02).
         05  MEI-ST2         PIC  X(04).
*    03  SOK-STATUS.
*        05  SOK-ST1         PIC  X(02).
*        05  SOK-ST2         PIC  X(04).
*    03  KBM-STATUS.
*        05  KBM-ST1         PIC  X(02).
*        05  KBM-ST2         PIC  X(04).
*画面退避用
*    COPY   FZA00851  OF XMDLIB  JOINING   SAV  AS   PREFIX.
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
 01  FILLER                  REDEFINES   WK-GYONO.
     03  GYO-TBL             PIC  N(01)  OCCURS  6.
*----<< ﾃﾞｨｽﾌﾟﾚｲ ｶﾞｲﾀﾞﾝｽ ｴﾘｱ >>-*
*01  GUIDE-AREA.
*    03  G001                PIC  N(30)  VALUE
*        NC"_取消_終了".
*    03  G002                PIC  N(30)  VALUE
*        NC"_取消_終了_項目戻り".
*    03  G003                PIC  N(30)  VALUE
*        NC"_取消_終了_項目戻り_前レコード_次レコード".
*01  MSG-AREA.
*    03  MSG01               PIC  N(20)  VALUE
*                   NC"表示中のＰＦキー以外は使用できません".
*    03  MSG02               PIC  N(20)  VALUE
*                   NC"伝票_に誤りがあります".
*    03  MSG03               PIC  N(20)  VALUE
*                   NC"伝票区分に誤りがあります．".
*    03  MSG04               PIC  N(20)  VALUE
*                   NC"入出庫日の入力に誤りがあります．".
*    03  MSG05               PIC  N(20)  VALUE
*                   NC"場所に誤りがあります．".
*    03  MSG06               PIC  N(20)  VALUE
*                   NC"数量に誤りがあります．".
*    03  MSG07               PIC  N(20)  VALUE
*                   NC"ＹまたはＨを入力して下さい．".
*    03  MSG08               PIC  N(20)  VALUE
*                  NC"入力された伝票_は既に登録されています．".
*    03  MSG09               PIC  N(20)  VALUE
*                   NC"入力された場所は存在しません．".
*    03  MSG10               PIC  N(20)  VALUE
*                   NC"入力された伝票_は存在しません．".
*    03  MSG11               PIC  N(20)  VALUE
*                   NC"削除区分に誤りがあります．".
*    03  MSG12               PIC  N(20)  VALUE
*                   NC"入力された商品は未登録です．".
*    03  MSG13               PIC  N(20)  VALUE
*                   NC"次レコードは存在しません．".
*    03  MSG14               PIC  N(20)  VALUE
*                   NC"前レコードは存在しません．".
*    03  MSG15               PIC  N(20)  VALUE
*                   NC"処理区分に誤りがあります．".
*    03  MSG16               PIC  N(20)  VALUE
*                   NC"作業区分に誤りがあります．".
*    03  MSG17               PIC  N(20)  VALUE
*                   NC"明細を入力して下さい．".
*    03  MSG18               PIC  N(20)  VALUE
*                   NC"商品在庫マスタに未登録です．".
*    03  MSG19               PIC  N(20)  VALUE
*                   NC"担当者に誤りがあります．".
*    03  MSG20               PIC  N(20)  VALUE
*                   NC"未出庫フラグに誤りがあります".
*    03  MSG21               PIC  N(20)  VALUE
*                   NC"部門コードに誤りがあります".
*    03  MSG22               PIC  N(20)  VALUE
*                   NC"部門コードが未登録です".
*    03  MSG23               PIC  N(20)  VALUE
*                   NC"倉庫コードが条件マスタに未登録".
*    03  MSG24               PIC  N(20)  VALUE
*                   NC"異なる部門ＣＤは指定できません".
*01  FILLER                  REDEFINES   MSG-AREA.
*    03  MSG-TBL             PIC  N(20)  OCCURS       24.
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
     03  NYU-FLG             PIC  9(01)  VALUE  ZERO.
     03  ERR-FLG             PIC  9(02)  VALUE  ZERO.
     03  SYR-FLG             PIC  9(02)  VALUE  ZERO.
     03  CHK-FLG             PIC  9(01)  VALUE  ZERO.
     03  DEL-FLG             PIC  9(01)  OCCURS  6.
     03  ZAI-FLG             PIC  9(01).
     03  HJYOKEN-INV-FLG     PIC  X(03)  VALUE  SPACE.
*----<< ｶｳﾝﾄ ｴﾘｱ >>----
 01  COUNTERS.
     03  MAX-LINE            PIC  9(02)  VALUE  ZERO.
     03  GYO-CNT             PIC  9(01)  VALUE  ZERO.
*----<< ﾋﾝﾀﾝ ﾜｰｸ ｴﾘｱ >>----
 01  WK-HINT-AREA.
     03  WK-HINTI-X.
         05  WK-HINTI        PIC  X(01)  OCCURS  5.
     03  WK-HINTO-X.
         05  WK-HINTO        PIC  X(01)  OCCURS  5.
     03  WK-HINT             PIC  X(05).
*----<< ｷｰ ﾜｰｸ ｴﾘｱ >>----
 01  WK-KEY-AREA.
     03  WK-F02              PIC  9(07).
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
*倉庫＋部門保存
 01  WK-SOKCD                     PIC  X(02)  VALUE  SPACE.
 01  WK-BUMON-CD                  PIC  9(04)  VALUE  ZERO.
 01  HEN-BUMON-CD                 PIC  9(04)  VALUE  ZERO.
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
 01  OUT-DENNO             PIC 9(07).
*---<< ｻﾌﾞﾙｰﾁﾝ LINK AREA >>-*
 LINKAGE        SECTION.
 01  LINK-AREA.
     03  LINK-IN.
         05  LI-WSMEI        PIC  X(08).
*
******************************************************************
 PROCEDURE           DIVISION    USING   LINK-AREA.
******************************************************************
*--------------------------------------------------------------*
*    LEVEL   0     エラー処理　　　　　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 DECLARATIVES.
*----------  入出庫ファイル  ----------------------------------*
 NYU-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE   NYSFILF.
     ACCEPT      SYS-DATE    FROM  DATE.
     ACCEPT      SYS-TIME    FROM  TIME.
     DISPLAY  "### " PGM-ID " " NC"入出庫ファイル異常！"
              "ST1=" NYU-ST1                 " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"  UPON STAT.
*
     ACCEPT   IN-DATA        FROM STAT.
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
     STOP     RUN.
*----------   倉庫マスタ　-----------------------------------*
*AOK-ERR                     SECTION.
*    USE         AFTER       EXCEPTION PROCEDURE   ZSOKMS.
*    ACCEPT      SYS-DATE    FROM  DATE.
*    ACCEPT      SYS-TIME    FROM  TIME.
*    DISPLAY  "### " PGM-ID " " NC"倉庫マスタ異常！"
*             "ST1=" SOK-ST1                 " "
*             SYS-YY "." SYS-MM "." SYS-DD " "
*             SYS-HH ":" SYS-MN ":" SYS-SS " ###"  UPON STAT.
*
*    ACCEPT   IN-DATA        FROM STAT.
*    STOP     RUN.
*----------    表示ファイル -----------------------------------*
*DSP-ERR                     SECTION.
*    USE         AFTER       EXCEPTION PROCEDURE   DSPFILE.
*    ACCEPT      SYS-DATE    FROM  DATE.
*    ACCEPT      SYS-TIME    FROM  TIME.
*    DISPLAY  "### " PGM-ID " " NC"表示ファイル異常！"
*             "ST1=" DSP-ST1                 " "
*             SYS-YY "." SYS-MM "." SYS-DD " "
*             SYS-HH ":" SYS-MN ":" SYS-SS " ###"  UPON STAT.
*
*    ACCEPT   IN-DATA        FROM STAT.
*    STOP     RUN.
 END DECLARATIVES.
*--------------------------------------------------------------*
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 PROG-CNTL          SECTION.
     PERFORM  INIT-RTN.
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
     OPEN     I-O       NYSFILF.
     OPEN     I-O       ZAMZAIF.
     OPEN     INPUT     HJYOKEN.
     OPEN     INPUT     HMEIMS.
*    OPEN     INPUT     ZSOKMS.
*    OPEN     INPUT     SGYKBMF.
*    OPEN     I-O       DSPFILE.
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
*場所取得
*    MOVE     "65"                TO   JYO-F01.
*    MOVE     LI-WSMEI            TO   JYO-F02.
*    PERFORM  JYO-RD-RTN.
*    IF  INV-FLG  =  1
*        DISPLAY   "HJYOKEN INV KEY=65"  UPON STAT
*        MOVE      "END"     TO   END-FLG
*        GO   TO   INIT-EXIT
*    END-IF.
*    MOVE     JYO-F14(1:2)        TO   WK-BSYNO1.
*    MOVE     JYO-F15(1:2)        TO   WK-BSYNO2.
*
*特販部名称編集
*    MOVE     SPACE               TO   JYO-REC.
*    INITIALIZE                        JYO-REC.
*    MOVE    "99"                 TO   JYO-F01.
*    MOVE    "BUMON"              TO   JYO-F02.
*    READ     HJYOKEN
*      INVALID KEY
*             MOVE NC"＊＊＊＊＊＊"   TO   HEN-TOKHAN
*      NOT INVALID KEY
*             MOVE JYO-F03            TO   HEN-TOKHAN
*             MOVE JYO-F04            TO   HEN-BUMON-CD
*    END-READ.
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
         WHEN    1      PERFORM   DSP-SRKBN-RTN
         WHEN    2      PERFORM   DSP-GRP03-RTN
         WHEN    99     PERFORM   DSP-GRP04-RTN
         WHEN    3      PERFORM   DSP-DPNO-RTN
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
     CLOSE              NYSFILF
                        ZAMZAIF
                        HJYOKEN
                        HMEIMS.
*                       ZSOKMS
*                       SGYKBMF
*                       DSPFILE.
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
*    INITIALIZE         COUNTERS.
*    INITIALIZE         FLAGS.
*    MOVE     SPACE          TO   FZA00851.
*行_セット
*    PERFORM  VARYING  I  FROM  1  BY  1  UNTIL  I  >  6
*             MOVE  GYO-TBL(I)     TO  GYONO(I)
*    END-PERFORM.
*属性クリア
*    PERFORM  CLR-HEAD-RTN.
*    PERFORM  CLR-BODY-RTN.
*    PERFORM  CLR-TAIL-RTN.
*
*    MOVE     SPACE          TO   DSP-CNTL.
*    MOVE     "FZA00851"     TO   DSP-FMT.
*    MOVE     "ALLF"         TO   DSP-GRP.
* 処理区分の入力を行う
     MOVE     1              TO   SYR-FLG.
 DSP-INIT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      処理区分　入力　　　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 DSP-SRKBN-RTN          SECTION.
*    MOVE     G001           TO   GUIDE.
*    MOVE     "GRP001"       TO   WK-GRP.
*    PERFORM  DSP-RD-RTN.
* アテンション判定
*    EVALUATE DSP-FNC
*    WHEN     "F004"   MOVE      0         TO   SYR-FLG
*    WHEN     "F005"   MOVE     "END"      TO   END-FLG
*    WHEN     "E000"   PERFORM   CHK-SRKBN-RTN
*    WHEN     OTHER    MOVE      1         TO   ERR-FLG
*    END-EVALUATE.
*
 DSP-SRKBN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      処理区分　チェック　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 CHK-SRKBN-RTN          SECTION.
*属性クリア
*    PERFORM  CLR-HEAD-RTN.
*    IF  SRKBN   IS  NOT  NUMERIC
*        MOVE      0         TO   SRKBN
*    END-IF.
*    EVALUATE    SRKBN
*        WHEN    1
*                MOVE  NC"登録"    TO  SRMEI
*                PERFORM  CHK-DPNO-RTN
*                MOVE  2           TO  SYR-FLG
*        WHEN    2
*                MOVE  NC"修正"    TO  SRMEI
*                MOVE  3           TO  SYR-FLG
*        WHEN    3
*                MOVE  NC"削除"    TO  SRMEI
*                MOVE  3           TO  SYR-FLG
*        WHEN    9
*                MOVE  NC"照会"    TO  SRMEI
*                MOVE  3           TO  SYR-FLG
*        WHEN    OTHER
*                MOVE  15          TO  ERR-FLG
*                MOVE  "R"         TO  EDIT-OPTION OF SRKBN
*    END-EVALUATE.
 CHK-SRKBN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＧＲＰ００３　入力　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 DSP-GRP03-RTN       SECTION.
*    MOVE     G002           TO   GUIDE.
*    MOVE     "GRP003"       TO   WK-GRP.
*    PERFORM  DSP-RD-RTN.
*
*    EVALUATE DSP-FNC
*    WHEN     "F004"
*             MOVE      0         TO   SYR-FLG
*    WHEN     "F005"
*             MOVE      "END"     TO   END-FLG
*    WHEN     "F006"
*             MOVE      1         TO   SYR-FLG
*             PERFORM  CLR-HEAD-RTN
*    WHEN     "E000"
*             PERFORM  CHK-GRP03-RTN
*             IF     ERR-FLG   =   ZERO
*                    MOVE   4        TO  SYR-FLG
*             END-IF
*    WHEN     OTHER
*             MOVE      1         TO   ERR-FLG
*    END-EVALUATE.
*
 DSP-GRP03-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＧＲＰ００４　入力　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 DSP-GRP04-RTN       SECTION.
*    MOVE     G002           TO   GUIDE.
*    MOVE     "GRP004"       TO   WK-GRP.
*    PERFORM  DSP-RD-RTN.
*
*    EVALUATE DSP-FNC
*    WHEN     "F004"
*             MOVE      0         TO   SYR-FLG
*    WHEN     "F005"
*             MOVE      "END"     TO   END-FLG
*    WHEN     "F006"
*             MOVE      1         TO   SYR-FLG
*             PERFORM  CLR-HEAD-RTN
*    WHEN     "E000"
*             PERFORM  CLR-HEAD-RTN
*             PERFORM  CHK-DATE-RTN
*             IF     ERR-FLG   =   ZERO
*                    MOVE   4        TO  SYR-FLG
*             END-IF
*    WHEN     OTHER
*             MOVE      1         TO   ERR-FLG
*    END-EVALUATE.
*
 DSP-GRP04-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3
*--------------------------------------------------------------*
 CHK-DATE-RTN        SECTION.
*
*入出庫日
     IF  NYSYMD  IS       NOT  NUMERIC
         MOVE    0         TO  NYSYMD
     END-IF.
*日付論理チェック
     MOVE     "1"                 TO   LINK-IN-KBN.
     MOVE     NYSYMD              TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
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
         MOVE   "C"        TO   EDIT-CURSOR  OF  NYSYMD
         MOVE   "R"        TO   EDIT-OPTION  OF  NYSYMD
         GO      TO        CHK-DATE-EXT
     END-IF.
*
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     NYSYMD              TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE     LINK-OUT-YMD        TO   WK-NYS-DATE.
     MOVE     NYSYMD              TO   WK-NYSYMD.
*
     MOVE     SYS-DATE2           TO   WK-CHKS-DATE.
     MOVE     WK-NYS-DATE         TO   WK-CHKN-DATE.
     IF  (WK-CHKS-YYYY        NOT =    WK-CHKN-YYYY ) AND
         (WK-CHKS-YYYY - 1    NOT =    WK-CHKN-YYYY )
         IF  ERR-FLG       =   0
             MOVE      4       TO   ERR-FLG
         END-IF
         MOVE   "C"        TO   EDIT-CURSOR  OF  NYSYMD
         MOVE   "R"        TO   EDIT-OPTION  OF  NYSYMD
         GO      TO        CHK-DATE-EXT
     END-IF.
     IF  (WK-CHKS-YYYY        NOT =    WK-CHKN-YYYY )  AND
         (WK-CHKS-MM          NOT =    1          )
         IF  (WK-CHKS-YYYY - 1    NOT =    WK-CHKN-YYYY )  AND
             (WK-SYS-MMR          NOT =    12         )
         IF  ERR-FLG       =   0
             MOVE      4       TO   ERR-FLG
         END-IF
         MOVE   "C"        TO   EDIT-CURSOR  OF  NYSYMD
         MOVE   "R"        TO   EDIT-OPTION  OF  NYSYMD
         GO      TO        CHK-DATE-EXT
         END-IF
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
     EVALUATE   NYSYMD(3:2)
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
     IF  WK-SYS-DATE   >   WK-SIME
         IF  ERR-FLG       =   0
             MOVE   4          TO   ERR-FLG
         END-IF
         MOVE   "C"        TO   EDIT-CURSOR  OF  NYSYMD
         MOVE   "R"        TO   EDIT-OPTION  OF  NYSYMD
     END-IF.
     IF  WK-SYS-DATE   <   WK-NYS-DATE
         IF  ERR-FLG       =   0
             MOVE   4          TO   ERR-FLG
         END-IF
         MOVE   "C"        TO   EDIT-CURSOR  OF  NYSYMD
         MOVE   "R"        TO   EDIT-OPTION  OF  NYSYMD
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
     IF  ( WK-HANI2   <=  WK-NYS-DATE )
         CONTINUE
     ELSE
         IF  ERR-FLG       =   0
             MOVE   4          TO   ERR-FLG
         END-IF
         MOVE   "C"        TO   EDIT-CURSOR  OF  NYSYMD
         MOVE   "R"        TO   EDIT-OPTION  OF  NYSYMD
     END-IF.
*
 CHK-DATE-EXT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＧＲＰ００３　チェック　　　　　　　　　　　*
*--------------------------------------------------------------*
 CHK-GRP03-RTN      SECTION.
*属性クリア
*    PERFORM  CLR-HEAD-RTN.
***** 入力チェック *****
*伝票区分
*    IF     DPKBN   IS     NOT   NUMERIC
*           MOVE    0       TO   DPKBN
*    END-IF.
*    EVALUATE   DPKBN
*        WHEN   31     MOVE  NC"入庫" TO  DPMEI
*        WHEN   32     MOVE  NC"出庫" TO  DPMEI
*        WHEN   OTHER  MOVE  3        TO  ERR-FLG
*                      MOVE  "C"      TO  EDIT-CURSOR  OF  DPKBN
*                      MOVE  "R"      TO  EDIT-OPTION  OF  DPKBN
*    END-EVALUATE.
*担当者追加
*    IF     TANTO       =    SPACE
*           IF  ERR-FLG            =  0
*               MOVE    19        TO  ERR-FLG
*           END-IF
*           MOVE    "C"       TO  EDIT-CURSOR  OF  TANTO
*           MOVE    "R"       TO  EDIT-OPTION  OF  TANTO
*    END-IF.
*作業区分
*    MOVE    SPACE            TO  SGMEI.
*    MOVE    SGKBN            TO  KBM-F01.
*    PERFORM     KBM-READ-RTN.
*    IF  INV-FLG            =  1
*        IF  ERR-FLG            =  0
*            MOVE    16        TO  ERR-FLG
*        END-IF
*        MOVE    "C"       TO  EDIT-CURSOR  OF  SGKBN
*        MOVE    "R"       TO  EDIT-OPTION  OF  SGKBN
*    ELSE
*        IF      KBM-F03   NOT =  2
*            IF  ERR-FLG            =  0
*                MOVE    16        TO  ERR-FLG
*            END-IF
*            MOVE    "C"       TO  EDIT-CURSOR  OF  SGKBN
*            MOVE    "R"       TO  EDIT-OPTION  OF  SGKBN
*        ELSE
*            MOVE    KBM-F02   TO  SGMEI
*        END-IF
*    END-IF.
*伝区３１で作業区分Ｉ３はエラー
*    IF  DPKBN  =  31  AND  SGKBN  =  "I3"
*        IF  ERR-FLG            =  0
*            MOVE    16        TO  ERR-FLG
*        END-IF
*        MOVE    "C"       TO  EDIT-CURSOR  OF  SGKBN
*        MOVE    "R"       TO  EDIT-OPTION  OF  SGKBN
*    END-IF.
*
*入出庫日
*    PERFORM   CHK-DATE-RTN.
*
*入出庫場所
*伝区３２かつ作業区分Ｉ３以外，場所両方入力はエラー
*    MOVE   SPACE          TO  BSYM2.
*    IF  DPKBN   =  32
*        IF  SGKBN  NOT  =  "I3"
*            IF  BASY01  NOT  =  SPACE  AND
*                BASY02  NOT  =  SPACE
*                IF   ERR-FLG              =  0
*                     MOVE  5           TO  ERR-FLG
*                END-IF
*                MOVE  "C"         TO  EDIT-CURSOR  OF  BASY02
*                MOVE  "R"         TO  EDIT-OPTION  OF  BASY02
*                GO    TO          CHK-GRP03-EXIT
*            END-IF
*        ELSE
*            IF  BASY01        =  SPACE
*                IF   ERR-FLG              =  0
*                     MOVE  5           TO  ERR-FLG
*                END-IF
*                MOVE  "C"         TO  EDIT-CURSOR  OF  BASY01
*                MOVE  "R"         TO  EDIT-OPTION  OF  BASY01
*                GO    TO          CHK-GRP03-EXIT
*            END-IF
*            IF  BASY02        =  SPACE
*                IF   ERR-FLG              =  0
*                     MOVE  5           TO  ERR-FLG
*                END-IF
*                MOVE  "C"         TO  EDIT-CURSOR  OF  BASY02
*                MOVE  "R"         TO  EDIT-OPTION  OF  BASY02
*                GO    TO          CHK-GRP03-EXIT
*            END-IF
*        END-IF
*    ELSE
*        IF  BASY01  NOT  =  SPACE  AND
*            BASY02  NOT  =  SPACE
*            IF   ERR-FLG              =  0
*                 MOVE  5           TO  ERR-FLG
*            END-IF
*            MOVE  "C"         TO  EDIT-CURSOR  OF  BASY02
*            MOVE  "R"         TO  EDIT-OPTION  OF  BASY02
*            GO    TO          CHK-GRP03-EXIT
*    END-IF.
*場所１は必須入力
*    IF     BASY01    =  SPACE
*        IF   ERR-FLG    =    0
*             MOVE    5    TO  ERR-FLG
*        END-IF
*        MOVE  "C"         TO  EDIT-CURSOR  OF  BASY01
*        MOVE  "R"         TO  EDIT-OPTION  OF  BASY01
*        GO    TO          CHK-GRP03-EXIT
*    END-IF.
*場所１・場所２が同じならエラー
*    IF     BASY01    =  BASY02
*        IF   ERR-FLG    =    0
*             MOVE    5    TO  ERR-FLG
*        END-IF
*        IF   WK-BSYNO1  =   "01"    OR
*             WK-BSYNO2  =   "01"
*             MOVE  "C"    TO  EDIT-CURSOR  OF  BASY01
*             MOVE  "R"    TO  EDIT-OPTION  OF  BASY01
*        END-IF
*        MOVE  "C"         TO  EDIT-CURSOR  OF  BASY02
*        MOVE  "R"         TO  EDIT-OPTION  OF  BASY02
*        GO    TO          CHK-GRP03-EXIT
*    END-IF.
* 場所１存在チェック
*    IF  WK-BSYNO1       =    "01"    OR
*        WK-BSYNO2       =    "01"
*        MOVE         SPACE       TO  BSYM1
*        MOVE         BASY01      TO  SOK-F01
*        PERFORM      SOK-READ-RTN
*        IF  INV-FLG            =  1
*            IF  ERR-FLG            =  0
*                MOVE  9          TO  ERR-FLG
*            END-IF
*            MOVE  "C"        TO  EDIT-CURSOR  OF  BASY01
*            MOVE  "R"        TO  EDIT-OPTION  OF  BASY01
*            GO    TO         CHK-GRP03-EXIT
*        END-IF
*        MOVE   SOK-F02           TO  BSYM1
*    END-IF.
*場所１部門ＣＤ取得
*    IF    BASY01  NOT =  SPACE
*    AND   BUCD1       =  SPACE
*          MOVE   BASY01   TO  WK-SOKCD
*          PERFORM HJYOKEN-READ-SEC
*          IF  HJYOKEN-INV-FLG = SPACE
*              MOVE   WK-BUMON-CD TO BUCD1
*          END-IF
*    END-IF.
*場所１部門ＣＤ存在チェック
*    IF    BASY01  NOT =  SPACE
*    AND   BUCD1   NOT =  SPACE
*          MOVE    22      TO  JYO-F01
*          MOVE    BUCD1   TO  JYO-F02
*          READ    HJYOKEN
*             INVALID KEY
*                  IF   ERR-FLG    =    0
*                       MOVE    22   TO  ERR-FLG
*                  END-IF
*                  MOVE  "C"         TO  EDIT-CURSOR  OF  BUCD1
*                  MOVE  "R"         TO  EDIT-OPTION  OF  BUCD1
*                  GO    TO          CHK-GRP03-EXIT
*             NOT INVALID
*                  MOVE   JYO-F03    TO  BUNM1
*          END-READ
*          MOVE    JYO-F03 TO  BUNM1
*          MOVE    20      TO  JYO-F01
*          MOVE    BASY01  TO  JYO-F02
*          READ    HJYOKEN
*             INVALID KEY
*                  IF   ERR-FLG    =    0
*                       MOVE    23   TO  ERR-FLG
*                  END-IF
*                  MOVE  "C"         TO  EDIT-CURSOR  OF  BASY01
*                  MOVE  "R"         TO  EDIT-OPTION  OF  BASY01
*                  GO    TO          CHK-GRP03-EXIT
*          END-READ
*          MOVE    1     TO          INV-FLG
*          IF      BUCD1  =  JYO-F08
*                  MOVE   ZERO   TO  INV-FLG
*          ELSE
*           IF      BUCD1  =   JYO-F09
*                   MOVE   ZERO   TO  INV-FLG
*           ELSE
*            IF      BUCD1  =   JYO-F10
*                    MOVE   ZERO   TO  INV-FLG
*            ELSE
*             IF      BUCD1  =   JYO-F11
*                     MOVE   ZERO   TO  INV-FLG
*             ELSE
*              IF      BUCD1  =   JYO-F12
*                  MOVE   ZERO   TO  INV-FLG
*              END-IF
*             END-IF
*            END-IF
*           END-IF
*          END-IF
*          IF      INV-FLG      =     1
*                  IF   ERR-FLG    =    0
*                       MOVE    21   TO  ERR-FLG
*                  END-IF
*                  MOVE  "C"         TO  EDIT-CURSOR  OF  BUCD1
*                  MOVE  "R"         TO  EDIT-OPTION  OF  BUCD1
*                  GO    TO          CHK-GRP03-EXIT
*          END-IF
*    END-IF.
* 場所２存在チェック
*    IF  BASY02      NOT =    SPACE
*        MOVE  BASY02      TO  SOK-F01
*        PERFORM           SOK-READ-RTN
*        IF  INV-FLG            =  1
*            IF  ERR-FLG            =  0
*                MOVE  9           TO  ERR-FLG
*            END-IF
*            MOVE  "C"         TO  EDIT-CURSOR  OF  BASY02
*            MOVE  "R"         TO  EDIT-OPTION  OF  BASY02
*            GO    TO          CHK-GRP03-EXIT
*        END-IF
*        MOVE  SOK-F02     TO  BSYM2
*    END-IF.
*場所２部門ＣＤ取得
*    IF    BASY02  NOT =  SPACE
*    AND   BUCD2       =  SPACE
*          MOVE   BASY02   TO  WK-SOKCD
*          PERFORM HJYOKEN-READ-SEC
*          IF  HJYOKEN-INV-FLG = SPACE
*              MOVE   WK-BUMON-CD TO BUCD2
*          END-IF
*    END-IF.
*
*場所２部門ＣＤ存在チェック
*    IF    BASY02  NOT =  SPACE
*   AND    BUCD2   NOT =  SPACE
*          MOVE    22      TO  JYO-F01
*          MOVE    BUCD2   TO  JYO-F02
*          READ    HJYOKEN
*             INVALID KEY
*                  IF   ERR-FLG    =    0
*                       MOVE    22   TO  ERR-FLG
*                  END-IF
*                  MOVE  "C"         TO  EDIT-CURSOR  OF  BUCD2
*                  MOVE  "R"         TO  EDIT-OPTION  OF  BUCD2
*                  GO    TO          CHK-GRP03-EXIT
*             NOT INVALID
*                  MOVE   JYO-F03    TO  BUNM2
*          END-READ
*          MOVE    JYO-F03 TO  BUNM2
*          MOVE    20      TO  JYO-F01
*          MOVE    BASY02  TO  JYO-F02
*          READ    HJYOKEN
*             INVALID KEY
*                  IF   ERR-FLG    =    0
*                       MOVE    23   TO  ERR-FLG
*                  END-IF
*                  MOVE  "C"         TO  EDIT-CURSOR  OF  BASY02
*                  MOVE  "R"         TO  EDIT-OPTION  OF  BASY02
*                  GO    TO          CHK-GRP03-EXIT
*          END-READ
*          MOVE    1     TO          INV-FLG
*          IF      BUCD2  =  JYO-F08
*                  MOVE   ZERO   TO  INV-FLG
*          ELSE
*           IF      BUCD2  =   JYO-F09
*                   MOVE   ZERO   TO  INV-FLG
*           ELSE
*            IF      BUCD2  =   JYO-F10
*                    MOVE   ZERO   TO  INV-FLG
*            ELSE
*             IF      BUCD2  =   JYO-F11
*                     MOVE   ZERO   TO  INV-FLG
*             ELSE
*              IF      BUCD2  =   JYO-F12
*                  MOVE   ZERO   TO  INV-FLG
*              END-IF
*             END-IF
*            END-IF
*           END-IF
*          END-IF
*          IF      INV-FLG      =     1
*                  IF   ERR-FLG    =    0
*                       MOVE    21   TO  ERR-FLG
*                  END-IF
*                  MOVE  "C"         TO  EDIT-CURSOR  OF  BUCD2
*                  MOVE  "R"         TO  EDIT-OPTION  OF  BUCD2
*                  GO    TO          CHK-GRP03-EXIT
*          END-IF
*    END-IF.
* 製品在庫移動の時の部門ＣＤチェック
*    IF  SGKBN  =  "I3"
*    AND BUCD1  NOT =  BUCD2
*        IF  ERR-FLG            =  0
*            MOVE  24         TO  ERR-FLG
*        END-IF
*        MOVE  "C"        TO  EDIT-CURSOR  OF  BUCD1
*        MOVE  "R"        TO  EDIT-OPTION  OF  BUCD1
*        MOVE  "R"        TO  EDIT-OPTION  OF  BUCD2
*    END-IF.
 CHK-GRP03-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      伝票_　入力                                *
*--------------------------------------------------------------*
 DSP-DPNO-RTN     SECTION.
*
*    MOVE     G003           TO   GUIDE.
*    MOVE     "GRP002"       TO   WK-GRP.
*    PERFORM  DSP-RD-RTN.
*アテンション判定
*    EVALUATE DSP-FNC
*    WHEN     "F004"
*             MOVE    0           TO   SYR-FLG
*    WHEN     "F005"
*             MOVE    "END"       TO   END-FLG
*    WHEN     "F006"
*             MOVE    1           TO   SYR-FLG
*             PERFORM  CLR-HEAD-RTN
*             MOVE  ZERO    TO   NYU-FLG
*    WHEN     "E000"
*             PERFORM   CHK-DPNO-RTN
*             IF     ERR-FLG   =   ZERO
*                    IF   SRKBN     =   2
*                         MOVE   99     TO   SYR-FLG
*                    ELSE
*                         MOVE  "Y"     TO   KAKNIN
*                         MOVE  90      TO   SYR-FLG
*                    END-IF
*             ELSE
*                    MOVE  ZERO    TO   NYU-FLG
*             END-IF
*    WHEN     "F011"
*             PERFORM   MAE-REC-RTN
*             IF     ERR-FLG   =   ZERO
*                    IF   SRKBN     =   2
*                         MOVE   4      TO   SYR-FLG
*                    ELSE
*                         MOVE  "Y"     TO   KAKNIN
*                         MOVE  90      TO   SYR-FLG
*                    END-IF
*             END-IF
*    WHEN     "F012"
*             PERFORM   TUGI-REC-RTN
*             IF     ERR-FLG   =   ZERO
*                    IF   SRKBN     =   2
*                         MOVE   4      TO   SYR-FLG
*                    ELSE
*                         MOVE  "Y"     TO   KAKNIN
*                         MOVE  90      TO   SYR-FLG
*                    END-IF
*             END-IF
*    WHEN     OTHER
*             MOVE      1         TO   ERR-FLG
*    END-EVALUATE.
*
 DSP-DPNO-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      伝票_　自動採番　　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 DPNO-SET-RTN           SECTION.
* 伝票_を自動採番する
*
     CALL   "SKYNSCKB"     USING    OUT-DENNO.
     MOVE   OUT-DENNO               TO   DPNO.
*
     IF     DATE-FLG   =     ZERO
            MOVE   SYS-DATE         TO   NYSYMD
     ELSE
            MOVE   ZERO             TO   DATE-FLG
     END-IF.
*
     IF     WK-BSYNO1      =  "01"    OR
            WK-BSYNO2      =  "01"
            CONTINUE
     ELSE
            MOVE    WK-BSYNO1       TO   BASY01
            MOVE    "X"             TO   EDIT-STATUS OF BASY01
            MOVE    WK-BSYNO1       TO   SOK-F01
            PERFORM  SOK-READ-RTN
            IF      INV-FLG  =  0
                    MOVE    SOK-F02    TO  BSYM1
            ELSE
                    MOVE    SPACE      TO  BSYM1
            END-IF
     END-IF.
 DPNO-SET-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      存在チェック処理（伝票_）                  *
*--------------------------------------------------------------*
 CHK-DPNO-RTN           SECTION.
*全レコード解放（排他制御用）
     CLOSE    NYSFILF.
     OPEN     I-O   NYSFILF.
* ヘッド部属性クリア
     PERFORM   CLR-HEAD-RTN.
*
     IF       DPNO      IS  NOT  NUMERIC
              MOVE  0        TO  DPNO
     END-IF.
* 登録時，伝票_を自動採番する
     IF       SRKBN           =  1
              PERFORM    DPNO-SET-RTN
     END-IF.
* 入出庫ファイル，存在チェック
     MOVE     DPNO           TO   NYU-F02.
     MOVE     0              TO   NYU-F03.
     START    NYSFILF   KEY  >=   NYU-F02  NYU-F03
         INVALID    KEY
                MOVE   1       TO   NYU-FLG
         NOT INVALID KEY
                PERFORM        NYU-READ-RTN
     END-START.
* 登録以外の時
     IF      (NYU-FLG         =   1)  AND
             (SRKBN       NOT =   1)
             MOVE   2        TO   ERR-FLG
             MOVE   "C"      TO   EDIT-CURSOR  OF  DPNO
             MOVE   "R"      TO   EDIT-OPTION  OF  DPNO
             GO  TO   CHK-DPNO-EXIT
     END-IF.
* 登録の時
     IF      (NYU-FLG         =   0)  AND
             (SRKBN           =   1)
             MOVE   8        TO   ERR-FLG
             MOVE   "C"      TO   EDIT-CURSOR  OF  DPNO
             MOVE   "R"      TO   EDIT-OPTION  OF  DPNO
             GO  TO   CHK-DPNO-EXIT
     END-IF.
     IF      SRKBN            =   1
             GO  TO   CHK-DPNO-EXIT
     END-IF.
* 該当データを画面にセットする
*    MOVE    SRKBN           TO   SAV-SRKBN.
*    MOVE    SRMEI           TO   SAV-SRMEI.
*    MOVE    HEAD1           TO   SAV-HEAD1.
*    MOVE    SPACE           TO   FZA00851.
*    MOVE    SAV-SRKBN       TO   SRKBN.
*    MOVE    SAV-SRMEI       TO   SRMEI.
*    MOVE    SAV-HEAD1       TO   HEAD1.
*    PERFORM  VARYING  I  FROM  1  BY  1  UNTIL  I   >  6
*            MOVE   GYO-TBL(I)     TO  GYONO(I)
*    END-PERFORM.
*    PERFORM     NYU-DSP-RTN.
*
 CHK-DPNO-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      前レコードチェック処理                      *
*--------------------------------------------------------------*
*MAE-REC-RTN            SECTION.
*全レコード解放（排他制御用）
*    CLOSE    NYSFILF.
*    OPEN     I-O   NYSFILF.
* 属性クリア
*    MOVE     ZERO      TO  NYU-FLG.
*    PERFORM   CLR-HEAD-RTN.
*
*    IF       DPNO      IS  NOT  NUMERIC
*             MOVE  0        TO  DPNO
*    END-IF.
* 伝票_存在チェック
*    MOVE     DPNO           TO  NYU-F02.
*    MOVE     0              TO  NYU-F03.
*    START    NYSFILF   KEY  IS  <  NYU-F02  NYU-F03
*                WITH   REVERSED  ORDER
*        INVALID  KEY
*             MOVE  1        TO  NYU-FLG
*        NOT  INVALID  KEY
*             PERFORM  NYU-READ1-RTN
*             IF  NYU-FLG           =  0
*                 MOVE    NYU-F02     TO  WK-F02
*                 MOVE    WK-F02      TO  NYU-F02
*                 MOVE    0           TO  NYU-F03
*                 START    NYSFILF   KEY  >=   NYU-F02  NYU-F03
*                     INVALID    KEY
*                          MOVE   1       TO   NYU-FLG
*                     NOT INVALID KEY
*                         PERFORM        NYU-READ1-RTN
*                 END-START
*             END-IF
*    END-START.
*    IF      NYU-FLG          =   1
*            MOVE   14       TO   ERR-FLG
*            MOVE   "C"      TO   EDIT-CURSOR  OF  DPNO
*            MOVE   "R"      TO   EDIT-OPTION  OF  DPNO
*            GO  TO          MAE-REC-EXIT
*    END-IF.
* 該当データを画面にセット
*    MOVE    SRKBN           TO   SAV-SRKBN.
*    MOVE    SRMEI           TO   SAV-SRMEI.
*    MOVE    HEAD1           TO   SAV-HEAD1.
*    MOVE    SPACE           TO   FZA00851.
*    MOVE    SAV-SRKBN       TO   SRKBN.
*    MOVE    SAV-SRMEI       TO   SRMEI.
*    MOVE    SAV-HEAD1       TO   HEAD1.
*    PERFORM  VARYING   I  FROM  1  BY  1
*               UNTIL   I     >  6
*            MOVE  GYO-TBL(I)   TO  GYONO(I)
*    END-PERFORM.
*    PERFORM     NYU-DSP-RTN.
*
*MAE-REC-EXIT.
*    EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      次レコードチェック処理                      *
*--------------------------------------------------------------*
*TUGI-REC-RTN           SECTION.
*全レコード解放（排他制御用）
*    CLOSE    NYSFILF.
*    OPEN     I-O   NYSFILF.
* 属性クリア
*    MOVE     ZERO      TO  NYU-FLG.
*    PERFORM   CLR-HEAD-RTN.
*
*    IF       DPNO      IS  NOT  NUMERIC
*             MOVE  0        TO  DPNO
*    END-IF.
* 登録処理の場合エラー
*    IF       SRKBN           =  1
*             MOVE  1        TO  ERR-FLG
*             MOVE  "C"      TO  EDIT-CURSOR  OF  DPNO
*             MOVE  "R"      TO  EDIT-OPTION  OF  DPNO
*             GO    TO       TUGI-REC-EXIT
*    END-IF.
* 伝票_存在チェック
*    MOVE     DPNO           TO  NYU-F02.
*    MOVE     99             TO  NYU-F03.
*    START    NYSFILF   KEY   >  NYU-F02  NYU-F03
*        INVALID  KEY
*             MOVE  1        TO  NYU-FLG
*        NOT  INVALID  KEY
*             PERFORM  NYU-READ1-RTN
*    END-START.
*    IF  NYU-FLG          =   1
*        MOVE   13       TO   ERR-FLG
*        MOVE   "C"      TO   EDIT-CURSOR  OF  DPNO
*        MOVE   "R"      TO   EDIT-OPTION  OF  DPNO
*        IF  SRKBN            =   1
*            MOVE    SRKBN           TO   SAV-SRKBN
*            MOVE    SRMEI           TO   SAV-SRMEI
*            MOVE    SPACE           TO   FZA00851
*            MOVE    SAV-SRKBN       TO   SRKBN
*            MOVE    SAV-SRMEI       TO   SRMEI
*        END-IF
*        GO  TO          TUGI-REC-EXIT
*    END-IF.
* 該当データを画面にセット
*    IF    SRKBN            =   1
*          MOVE    SPACE           TO   SAV-HEAD1
*    ELSE
*          MOVE    HEAD1           TO   SAV-HEAD1
*    END-IF.
*    MOVE    SRKBN           TO   SAV-SRKBN.
*    MOVE    SRMEI           TO   SAV-SRMEI.
*    MOVE    SPACE           TO   FZA00851.
*    MOVE    SAV-SRKBN       TO   SRKBN.
*    MOVE    SAV-SRMEI       TO   SRMEI.
*    MOVE    SAV-HEAD1       TO   HEAD1.
*    PERFORM  VARYING   I  FROM  1  BY  1
*               UNTIL   I     >  6
*            MOVE  GYO-TBL(I)   TO  GYONO(I)
*    END-PERFORM.
*    PERFORM     NYU-DSP-RTN.
*
*TUGI-REC-EXIT.
*    EXIT.
*--------------------------------------------------------------*
*    LEVEL  3         画面へ表示　　　　　　　　　　　　　　*
*--------------------------------------------------------------*
*NYU-DSP-RTN            SECTION.
*データ表示
*    PERFORM   NYU-TO-HEAD.
*    PERFORM   VARYING  I  FROM  1  BY  1  UNTIL    I  >  6
*                                    OR       NYU-FLG  =  1
*           IF    DPNO     =  NYU-F02
*                 ADD   1    TO   MAX-LINE
*                 IF    I        =  NYU-F03
*                       PERFORM   NYU-TO-BODY
*                       PERFORM   NYU-READ-RTN
*                       MOVE   0        TO   DEL-FLG(I)
*                 ELSE
*                       PERFORM   PRO-BODY-RTN
*                       MOVE   1        TO   DEL-FLG(I)
*                 END-IF
*           END-IF
*    END-PERFORM.
* 入力行以降 プロテクトセット
*    IF  I      <=     6
*        PERFORM    UNTIL    I   >   6
*              PERFORM   PRO-BODY-RTN
*              MOVE    1        TO   DEL-FLG(I)
*              ADD     1        TO   I
*        END-PERFORM
*    END-IF.
*NYU-DSP-EXIT.
*    EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＧＢＯＤＹ１　入力　　　　　　　　　　　　　*
*--------------------------------------------------------------*
*DSP-BODY-RTN     SECTION.
*    PERFORM  PRO-SET-RTN.
*
*    MOVE      G002          TO   GUIDE.
*    MOVE     "BODY"         TO   WK-GRP.
*    PERFORM  DSP-RD-RTN.
* アテンション判定
*    EVALUATE DSP-FNC
*    WHEN     "F004"
*             MOVE      0         TO   SYR-FLG
*    WHEN     "F005"
*             MOVE      "END"     TO   END-FLG
*    WHEN     "F006"
*             IF     SRKBN     =    1
*                    MOVE      2         TO   SYR-FLG
*             ELSE
*                    MOVE      3         TO   SYR-FLG
*             END-IF
*             PERFORM  CLR-BODY-RTN
*             MOVE   ZERO     TO   NYU-FLG
*    WHEN     "E000"
*             PERFORM  CLR-BODY-RTN
*             MOVE   ZERO         TO   GYO-CNT
*             PERFORM  CHK-BODY-RTN
*                           VARYING  L  FROM 1  BY  1
*                             UNTIL  L  >  6
*             IF   ERR-FLG  =  ZERO
*                  PERFORM  CLR-BODY-RTN
*                  MOVE    BODY1           TO  SAV-BODY1
*                  IF  SRKBN   =  1
*                      PERFORM   GYO-TUME-RTN
*                  END-IF
*                  PERFORM   VARYING  I   FROM  1  BY  1
*                              UNTIL  I      >  6
*                      MOVE    SAV-MEISAI(I)   TO  MEISAI(I)
*                      MOVE    GYO-TBL(I)      TO  GYONO(I)
*                  END-PERFORM
*                  PERFORM   CLR-TAIL-RTN
*                  MOVE      "Y"       TO   KAKNIN
*                  MOVE      90        TO   SYR-FLG
*---<  商品在庫マスタ存在チェック  >---*
*                  PERFORM  CLR-BODY-RTN
*                  PERFORM  ZAI-CHK-SEC
*             END-IF
*    WHEN     OTHER
*             MOVE      1         TO   ERR-FLG
*    END-EVALUATE.
*DSP-BODY-EXIT.
*    EXIT.
*--------------------------------------------------------------*
*    LEBEL  3       ＢＯＤＹ　チェック　　　　 　　　　　　　*
*--------------------------------------------------------------*
 CHK-BODY-RTN         SECTION.
*行削除チェック
*    IF     SRKBN          =    2    AND
*           DLKBN(L)       =   "9"
*           MOVE  SPACE        TO  MEISAI(L)  SAV-MEISAI(L)
*           MOVE  GYO-TBL(L)   TO  GYONO(L)   SAV-GYONO(L)
*           MOVE  1            TO  DEL-FLG(L)
*           GO  TO  CHK-BODY-90
*    END-IF.
* 未入力チェック
*     IF   (HIN101(L)            =   SPACE)   AND
*          (HIN201(L)            =   SPACE)   AND
*          (HIN301(L)            =   SPACE)   AND
*          (SYOCD(L)             =   SPACE)
*           IF  SRKBN   =    1
*               GO  TO  CHK-BODY-90
*           ELSE
*               IF (SURYOU(L)  NOT   NUMERIC ) OR
*                  (SURYOU(L)        =   ZERO)
*                   MOVE  SPACE      TO  MEISAI(L) SAV-MEISAI(L)
*                   MOVE  GYO-TBL(L) TO  GYONO(L)  SAV-GYONO(L)
*                   MOVE  1          TO  DEL-FLG(L)
*                   GO  TO  CHK-BODY-90
*                END-IF
*           END-IF
*     END-IF.
* 明細件数のカウント
*    ADD   1                    TO   GYO-CNT.
*    MOVE  0                    TO   DEL-FLG(L).
*
* 削除区分チェック
*     IF    SRKBN      NOT =  1
*           IF (DLKBN(L)   NOT =  ZERO)   AND
*              (DLKBN(L)   NOT =  9)      AND
*              (DLKBN(L)   NOT =  SPACE)
*               IF   ERR-FLG       =   ZERO
*                    MOVE  11    TO   ERR-FLG
*               END-IF
*               MOVE  "C"   TO   EDIT-CURSOR  OF  DLKBN(L)
*               MOVE  "R"   TO   EDIT-OPTION  OF  DLKBN(L)
*           END-IF
*     END-IF.
*** 商品コード右詰処理
*    PERFORM VARYING IXB    FROM  1   BY   1
*            UNTIL   IXB    >     7
*        PERFORM VARYING IXC    FROM  8   BY  -1
*                UNTIL   IXC    <     2
*            IF  SYOCD (L)(IXC:1)   =  SPACE
*                COMPUTE IXD    =     IXC   -   1
*                MOVE  SYOCD (L)(IXD:1)   TO   SYOCD (L)(IXC:1)
*                MOVE  SPACE              TO   SYOCD (L)(IXD:1)
*            END-IF
*        END-PERFORM
*    END-PERFORM.
**
*    PERFORM     VARYING    IXB   FROM    1   BY   1
*                UNTIL      (IXB   >      7 ) OR
*                (SYOCD (L)(IXB:1) NOT =  SPACE)
*        IF      SYOCD (L)(IXB:1)        =   SPACE
*                MOVE       "0"   TO      SYOCD (L)(IXB:1)
*        END-IF
*    END-PERFORM.
**
*品単数字右づめ変換
*５桁　頭空白詰
*    IF  HIN101(L)  NOT = SPACE   AND
*        HIN101(L)(5:1) = SPACE
*        MOVE   HIN101(L)  TO  WK-HINTI-X
*        MOVE   ZERO       TO  J
*        PERFORM VARYING I FROM 1 BY 1 UNTIL I > 5
*            IF  WK-HINTI(I)  NOT = SPACE
*                ADD    1   TO  J
*                MOVE   WK-HINTI(I)  TO  WK-HINTO(J)
*            END-IF
*        END-PERFORM
*        MOVE   SPACE            TO  WK-HINT
*        COMPUTE I = 6 - J
*        MOVE   WK-HINTO-X(1:J)  TO  WK-HINT(I:J)
*        MOVE   WK-HINT          TO  HIN101(L)
*    END-IF.
*商品名取得
     MOVE   SYOCD(L)         TO  MEI-F011.
     MOVE   HIN101(L)        TO  MEI-F0121.
     MOVE   HIN201(L)        TO  MEI-F0122.
     MOVE   HIN301(L)        TO  MEI-F0123.
     PERFORM   MEI-READ-RTN.
     IF   INV-FLG          =   1
          IF   ERR-FLG       =   ZERO
               MOVE  12      TO  ERR-FLG
          END-IF
          MOVE  "C"          TO  EDIT-CURSOR  OF SYOCD(L)
          MOVE  "R"          TO  EDIT-OPTION  OF SYOCD(L)
          MOVE  "R"          TO  EDIT-OPTION  OF HIN101(L)
          MOVE  "R"          TO  EDIT-OPTION  OF HIN201(L)
          MOVE  "R"          TO  EDIT-OPTION  OF HIN301(L)
     ELSE
          MOVE  MEI-F021     TO  WK-HINMEI1
          MOVE  MEI-F022     TO  WK-HINMEI2
          MOVE  WK-HINMEI    TO  HINMEI(L)
     END-IF.
*数量チェック
*    IF  SURYOU(L)     NOT  NUMERIC
*        MOVE   ZERO   TO   SURYOU(L)
*    END-IF.
*    IF  SURYOU(L)        =   ZERO    AND
*        SRKBN            =   1
*        IF    ERR-FLG      =   ZERO
*              MOVE   6     TO  ERR-FLG
*        END-IF
*        MOVE  "C"          TO  EDIT-CURSOR  OF SURYOU(L)
*        MOVE  "R"          TO  EDIT-OPTION  OF SURYOU(L)
*    END-IF.
*
* 登録時，全行未入力の場合１行目入力エラー
*CHK-BODY-90.
*    IF    SRKBN   =   1      AND
*          L       =   6      AND
*          GYO-CNT =   ZERO
*          MOVE   17            TO   ERR-FLG
*          MOVE   "C"           TO   EDIT-CURSOR  OF  SYOCD(1)
*          MOVE   "R"           TO   EDIT-OPTION  OF  SYOCD(1)
*          MOVE   "R"           TO   EDIT-OPTION  OF  HIN101(1)
*          MOVE   "R"           TO   EDIT-OPTION  OF  HIN201(1)
*          MOVE   "R"           TO   EDIT-OPTION  OF  HIN301(1)
*    END-IF.
 CHK-BODY-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEBEL  3       行番桁詰めチェック　　　　 　　　　　　　*
*--------------------------------------------------------------*
*GYO-TUME-RTN         SECTION.
* 登録時，空白行の行詰めを行う
*    MOVE   ZERO       TO  J.
*    MOVE   SPACE      TO  SAV-BODY1.
*      PERFORM   VARYING  I  FROM  1   BY  1
*                  UNTIL  I     >  6
*        IF   (SYOCD(I)     NOT =   SPACE)
*              ADD   1          TO   J
*              MOVE  MEISAI(I)  TO   SAV-MEISAI(J)
*        END-IF
*      END-PERFORM.
*GYO-TUME-EXIT.
*    EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      確認　入力　　　　　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 DSP-KAKNIN-RTN    SECTION.
     MOVE      G002            TO   GUIDE.
     MOVE     "KAK001"         TO   WK-GRP.
*    PERFORM  DSP-RD-RTN.
* アテンション判定
*    EVALUATE DSP-FNC
*    WHEN     "F004"
*             MOVE      0         TO   SYR-FLG
*    WHEN     "F005"
*             MOVE      "END"     TO   END-FLG
*    WHEN     "F006"
*             IF   SRKBN     =   1
*                  MOVE      4         TO   SYR-FLG
*                  MOVE      SAV-BODY1 TO   BODY1
*                  PERFORM   VARYING  I  FROM  1  BY  1
*                                                 UNTIL  I  >  6
*                       MOVE  "X"     TO  EDIT-STATUS  OF DLKBN(I)
*                  END-PERFORM
*             END-IF
*             IF   SRKBN     =    2
*                  MOVE      4         TO   SYR-FLG
*                  MOVE      SAV-BODY1 TO   BODY1
*                  PERFORM  VARYING  I  FROM  1  BY  1
*                                                UNTIL  I  >  6
*                     IF   (HIN101(I)   =   SPACE)  AND
*                          (HIN201(I)   =   SPACE)  AND
*                          (HIN301(I)   =   SPACE)  AND
*                          (SYOCD(I)    =   SPACE)
*                           PERFORM   PRO-BODY-RTN
*                     END-IF
*                 END-PERFORM
*             END-IF
*             IF     SRKBN     =     3   OR   9
*                    MOVE      3         TO   SYR-FLG
*             END-IF
*             MOVE      ZERO      TO   NYU-FLG
*             MOVE      ZERO      TO   CHK-FLG
*             PERFORM  VARYING   I  FROM  1  BY  1  UNTIL  I  > 6
*                  MOVE  GYO-TBL(I)   TO  GYONO(I)
*             END-PERFORM
*             PERFORM   CLR-TAIL-RTN
*    WHEN     "E000"
              PERFORM   CHK-KAKNIN-RTN
              IF   ERR-FLG  =  ZERO
                   IF  SRKBN   NOT = 9
                       PERFORM   NYU-UPDT-RTN
                   END-IF
                   PERFORM   KAKNIN-AFTER-RTN
              END-IF
*             PERFORM   CLR-TAIL-RTN
*             PERFORM   CLR-BODY-RTN
*    WHEN     OTHER
*             MOVE      1         TO   ERR-FLG
*    END-EVALUATE.
*
 DSP-KAKNIN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      確認　　　　チェック　　　　　　　　　　　　*
*--------------------------------------------------------------*
 CHK-KAKNIN-RTN     SECTION.
     MOVE     "M"        TO   EDIT-OPTION OF KAKNIN.
     IF      (KAKNIN  NOT  =  "Y") AND
             (KAKNIN  NOT  =  "H") AND
             (KAKNIN  NOT  =  "B")
              MOVE      7         TO   ERR-FLG
              MOVE     "R"        TO   EDIT-OPTION OF KAKNIN
     END-IF.
*
     IF      (KAKNIN       =  "B") AND
             (SRKBN    NOT =  1)
              MOVE      7         TO   ERR-FLG
              MOVE     "R"        TO   EDIT-OPTION OF KAKNIN
     END-IF.
 CHK-KAKNIN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      入出庫更新　処理　　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 NYU-UPDT-RTN          SECTION.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     NYSYMD              TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE     LINK-OUT-YMD        TO   WK-NYS-DATE.
*
     EVALUATE   SRKBN
         WHEN   1      PERFORM   NYU-WRITE
         WHEN   2      PERFORM   NYU-REWRITE
         WHEN   3      PERFORM   NYU-DELETE
     END-EVALUATE.
 NYU-UPDT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      確認後画面　処理　　　　　　　　　　　　　　*
*--------------------------------------------------------------*
*KAKNIN-AFTER-RTN          SECTION.
*
*    EVALUATE    KAKNIN
*        WHEN    "Y"
*                MOVE   SRKBN     TO   SAV-SRKBN
*                MOVE   SRMEI     TO   SAV-SRMEI
*                MOVE   SPACE     TO   FZA00851
*                PERFORM   VARYING   I  FROM  1  BY  1
*                            UNTIL   I     >  6
*                   MOVE   GYO-TBL(I)     TO   GYONO(I)
*                END-PERFORM
*                MOVE   SAV-SRKBN TO   SRKBN
*                MOVE   SAV-SRMEI TO   SRMEI
*                IF    SRKBN             =   1
*                      PERFORM   DPNO-SET-RTN
*                      MOVE   2         TO   SYR-FLG
*                ELSE
*                      MOVE   3         TO   SYR-FLG
*                END-IF
*
*        WHEN    "H"
*                IF    SRKBN             =   1
*                      MOVE   SRKBN      TO   SAV-SRKBN
*                      MOVE   SRMEI      TO   SAV-SRMEI
*                      MOVE   HEAD1      TO   SAV-HEAD1
*                      MOVE   SPACE      TO   FZA00851
*                      MOVE   SAV-SRKBN  TO   SRKBN
*                      MOVE   SAV-SRMEI  TO   SRMEI
*                      MOVE   SAV-HEAD1  TO   HEAD1
*                      MOVE   1          TO   DATE-FLG
*                      PERFORM   DPNO-SET-RTN
*                      MOVE   4          TO   SYR-FLG
*                ELSE
*                      PERFORM   TUGI-REC-RTN
*                      MOVE   SPACE      TO   BODY1
*                      MOVE   3          TO   SYR-FLG
*                END-IF
*                PERFORM   VARYING   I  FROM  1  BY  1
*                            UNTIL   I     >  6
*                   MOVE   GYO-TBL(I)    TO  GYONO(I)
*                END-PERFORM
*
*        WHEN    "B"
*                MOVE   SRKBN      TO  SAV-SRKBN
*                MOVE   SRMEI      TO  SAV-SRMEI
*                MOVE   BODY1      TO  SAV-BODY1
*                MOVE   NYSYMD     TO  SAV-NYSYMD
*                MOVE   SPACE      TO  FZA00851
*                MOVE   SAV-BODY1  TO  BODY1
*                MOVE   SAV-SRKBN  TO  SRKBN
*                MOVE   SAV-SRMEI  TO  SRMEI
*                MOVE   SAV-NYSYMD TO  NYSYMD
*                MOVE   1          TO  DATE-FLG
*                PERFORM   DPNO-SET-RTN
*                MOVE   2          TO  SYR-FLG
*    END-EVALUATE.
*KAKNIN-AFTER-EXIT.
*    EXIT.
*--------------------------------------------------------------*
*    LEVEL  4     入出庫ファイル登録処理　　　　　　　　　　　 *
*--------------------------------------------------------------*
 NYU-WRITE              SECTION.
*入出庫ファイル登録
     PERFORM  VARYING  I  FROM  1  BY  1  UNTIL  I  >  6
       IF  (SYOCD (I)      NOT  =  SPACE)
           PERFORM   DSP-TO-NYU
           PERFORM   ZAI-NEW-RTN
           WRITE     NYU-REC
       END-IF
     END-PERFORM.
 NYU-WRITE-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4     入出庫ファイル更新処理　　　　　　　　　　　 *
*--------------------------------------------------------------*
*NYU-REWRITE            SECTION.
*入出庫ファイル更新
*    MOVE   DPNO             TO   NYU-F02.
*    MOVE   0                TO   NYU-F03.
*    START    NYSFILF   KEY  >=   NYU-F02  NYU-F03
*      INVALID    KEY
*             MOVE   1       TO   NYU-FLG
*      NOT INVALID KEY
*             PERFORM        NYU-READ-RTN
*    END-START.
*    PERFORM  VARYING  I  FROM  1  BY  1
*               UNTIL  I     >  6  OR  NYU-FLG  =  1
*             IF  I            =  NYU-F03
*                 IF    DEL-FLG(I)    =   ZERO
*                       PERFORM    ZAI-OLD-RTN
*                       PERFORM    DSP-TO-NYU
*                       PERFORM    ZAI-NEW-RTN
*                       REWRITE    NYU-REC
*                 ELSE
*                       PERFORM    ZAI-OLD-RTN
*                       DELETE   NYSFILF  RECORD
*                 END-IF
*                 PERFORM    NYU-READ-RTN
*             END-IF
*    END-PERFORM.
*NYU-REWRITE-EXIT.
*    EXIT.
*--------------------------------------------------------------*
*    LEVEL  4     入出庫ファイル削除                           *
*--------------------------------------------------------------*
*NYU-DELETE             SECTION.
* 削除モードで処理した場合，削除フラグに１をセット
*    MOVE     0              TO   NYU-FLG.
*    MOVE     DPNO           TO   NYU-F02.
*    MOVE     0              TO   NYU-F03.
*    START    NYSFILF   KEY  >=   NYU-F02  NYU-F03
*      INVALID    KEY
*             MOVE   1       TO   NYU-FLG
*      NOT INVALID KEY
*             PERFORM        NYU-READ-RTN
*    END-START.
*    PERFORM  VARYING  I  FROM  1  BY  1
*               UNTIL  NYU-FLG  =  1  OR  I   >  6
*             IF   I            =  NYU-F03
*                  PERFORM    ZAI-OLD-RTN
*                  MOVE  1           TO  NYU-F96
*                  REWRITE    NYU-REC
*                  PERFORM    NYU-READ-RTN
*             END-IF
*    END-PERFORM.
*NYU-DELETE-EXIT.
*    EXIT.
*--------------------------------------------------------------*
*    LEVEL  4     表示項目を入出庫ファイルに転送               *
*--------------------------------------------------------------*
 DSP-TO-NYU             SECTION.
     IF    SRKBN    =    1
           MOVE  SPACE            TO   NYU-REC
           INITIALIZE                  NYU-REC
           MOVE   DPNO            TO   NYU-F02
           MOVE   I               TO   NYU-F03
           MOVE   SYS-DATE2       TO   NYU-F98
     ELSE
           MOVE   SYS-DATE2       TO   NYU-F99
     END-IF.
* 画面データを入出庫ファイルの項目にセット
     MOVE   DPKBN                 TO   NYU-F01.
     MOVE   SGKBN                 TO   NYU-F04.
     MOVE   SYOCD (I)             TO   NYU-F05.
     MOVE   HIN101(I)             TO   NYU-F06(1:5).
     MOVE   HIN201(I)             TO   NYU-F06(6:2).
     MOVE   HIN301(I)             TO   NYU-F06(8:1).
*
     IF  DPKBN      =  32
         IF  BASY02     =  SPACE
             MOVE  BASY01         TO   NYU-F10
             MOVE  SPACE          TO   NYU-F11
             MOVE  BUCD1          TO   NYU-F17
             MOVE  SPACE          TO   NYU-F18
         ELSE
             MOVE  BASY01         TO   NYU-F10
             MOVE  BASY02         TO   NYU-F11
             MOVE  BUCD1          TO   NYU-F17
             MOVE  BUCD2          TO   NYU-F18
         END-IF
         MOVE  TAN101(I)          TO   NYU-F07(1:1)
         MOVE  TAN201(I)          TO   NYU-F07(2:3)
         MOVE  TAN301(I)          TO   NYU-F07(5:2)
         MOVE  AIT101(I)          TO   NYU-F08(1:1)
         MOVE  AIT201(I)          TO   NYU-F08(2:3)
         MOVE  AIT301(I)          TO   NYU-F08(5:2)
     ELSE
         IF  BASY02     =  SPACE
             MOVE  SPACE          TO   NYU-F10
             MOVE  BASY01         TO   NYU-F11
             MOVE  SPACE          TO   NYU-F17
             MOVE  BUCD1          TO   NYU-F18
         ELSE
             MOVE  BASY02         TO   NYU-F10
             MOVE  BASY01         TO   NYU-F11
             MOVE  BUCD2          TO   NYU-F17
             MOVE  BUCD1          TO   NYU-F18
         END-IF
         MOVE  SPACE              TO   NYU-F10
         MOVE  BASY01             TO   NYU-F11
         MOVE  TAN101(I)          TO   NYU-F08(1:1)
         MOVE  TAN201(I)          TO   NYU-F08(2:3)
         MOVE  TAN301(I)          TO   NYU-F08(5:2)
     END-IF.
*
     MOVE   STK101(I)             TO   NYU-F09(1:5).
     MOVE   STK201(I)             TO   NYU-F09(6:1).
     MOVE   SURYOU(I)             TO   NYU-F12.
     MOVE   BIKOU(I)              TO   NYU-F13.
     MOVE   WK-NYS-DATE           TO   NYU-F15.
     MOVE   TANTO                 TO   NYU-F16.
***  MOVE   BUCD1                 TO   NYU-F17.
***  MOVE   BUCD2                 TO   NYU-F18.
     MOVE   WK-BSYNO1             TO   NYU-F97.
 DSP-TO-NYU-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4     ヘッド部読込　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
*NYU-TO-HEAD            SECTION.
* ヘッド部のデータを画面にセット
*    MOVE   NYU-F01          TO   DPKBN.
*    IF     NYU-F01           =   31
*           MOVE  NC"入庫"   TO   DPMEI
*    ELSE
*           MOVE  NC"出庫"   TO   DPMEI
*    END-IF.
*    MOVE   NYU-F02          TO   DPNO.
*    MOVE   NYU-F04          TO   SGKBN.
*    MOVE   NYU-F04          TO   KBM-F01.
*    PERFORM    KBM-READ-RTN.
*    IF     INV-FLG           =   1
*           MOVE   SPACE     TO   SGMEI
*    ELSE
*           MOVE   KBM-F02   TO   SGMEI
*    END-IF.
*    MOVE   NYU-F15(3:6)     TO   NYSYMD.
*    MOVE   NYU-F16          TO   TANTO.
* 倉庫場所のセット
*    IF  NYU-F10          =   SPACE
*        MOVE  NYU-F11    TO  BASY01   SOK-F01
*        MOVE  NYU-F18    TO  BUCD1
*        PERFORM    SOK-READ-RTN
*        IF  INV-FLG           =   1
*            MOVE  SPACE      TO   BSYM1
*        ELSE
*            MOVE  SOK-F02    TO   BSYM1
*        END-IF
*        MOVE  SPACE      TO   BASY02  BSYM2
*        GO  TO  NYU-TO-HEAD-EXIT
*    ELSE
*        MOVE  NYU-F10    TO   BASY01  SOK-F01
*        MOVE  NYU-F17    TO   BUCD1
*        PERFORM   SOK-READ-RTN
*        IF   INV-FLG           =   1
*             MOVE    SPACE    TO   BSYM1
*        ELSE
*             MOVE    SOK-F02  TO   BSYM1
*        END-IF
*    END-IF.
*    IF   NYU-F11           =   SPACE
*         MOVE  SPACE      TO   BASY02
*         MOVE  SPACE      TO   BSYM2
*    ELSE
*         MOVE  NYU-F11    TO   BASY02  SOK-F01
*         MOVE  NYU-F18    TO   BUCD2
*         PERFORM   SOK-READ-RTN
*         IF  INV-FLG           =   1
*             MOVE    SPACE    TO   BSYM2
*         ELSE
*             MOVE    SOK-F02  TO   BSYM2
*         END-IF
*    END-IF.
*NYU-TO-HEAD-EXIT.
*    EXIT.
*--------------------------------------------------------------*
*    LEVEL  4     ボディ部読込　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
*NYU-TO-BODY            SECTION.
* ボディ部データセット
*    IF     DPKBN             =   31
*           MOVE  NYU-F08(1:1)    TO   TAN101(I)
*           MOVE  NYU-F08(2:3)    TO   TAN201(I)
*           MOVE  NYU-F08(5:2)    TO   TAN301(I)
*    ELSE
*           MOVE  NYU-F07(1:1)    TO   TAN101(I)
*           MOVE  NYU-F07(2:3)    TO   TAN201(I)
*           MOVE  NYU-F07(5:2)    TO   TAN301(I)
*           MOVE  NYU-F08(1:1)    TO   AIT101(I)
*           MOVE  NYU-F08(2:3)    TO   AIT201(I)
*           MOVE  NYU-F08(5:2)    TO   AIT301(I)
*    END-IF.
*    MOVE   NYU-F05               TO   SYOCD (I).
*    MOVE   NYU-F06(1:5)          TO   HIN101(I).
*    MOVE   NYU-F06(6:2)          TO   HIN201(I).
*    MOVE   NYU-F06(8:1)          TO   HIN301(I).
*    MOVE   NYU-F12               TO   SURYOU(I).
*    MOVE   NYU-F09(1:5)          TO   STK101(I).
*    MOVE   NYU-F09(6:1)          TO   STK201(I).
*    MOVE   NYU-F13               TO   BIKOU (I).
* 商品名称取得
*    MOVE   NYU-F05               TO  MEI-F011.
*    MOVE   NYU-F06(1:5)          TO  MEI-F0121.
*    MOVE   NYU-F06(6:2)          TO  MEI-F0122.
*    MOVE   NYU-F06(8:1)          TO  MEI-F0123.
*    PERFORM   MEI-READ-RTN.
*    IF  INV-FLG                   =   1
*        MOVE  12                 TO  ERR-FLG
*        MOVE  "C"                TO  EDIT-CURSOR  OF SYOCD(I)
*        MOVE  "R"                TO  EDIT-OPTION  OF SYOCD(I)
*        MOVE  "R"                TO  EDIT-OPTION  OF HIN101(I)
*        MOVE  "R"                TO  EDIT-OPTION  OF HIN201(I)
*        MOVE  "R"                TO  EDIT-OPTION  OF HIN301(I)
*    ELSE
*        MOVE  MEI-F021           TO  WK-HINMEI1
*        MOVE  MEI-F022           TO  WK-HINMEI2
*        MOVE  WK-HINMEI          TO  HINMEI(I)
*    END-IF.
*NYU-TO-BODY-EXIT.
*    EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL     ﾃﾞｨｽﾌﾟﾚｰ  READ                              *
*--------------------------------------------------------------*
*DSP-RD-RTN           SECTION.
*
*    IF       ERR-FLG  =  0
*             MOVE      SPACE               TO   MESAGE
*    ELSE
*             MOVE      MSG-TBL (ERR-FLG)   TO   MESAGE
*    END-IF.
*    MOVE     "ALLF"         TO   DSP-GRP.
*    PERFORM  DSP-WRITE-RTN.
*
*    IF       ERR-FLG  NOT  =  0
*             MOVE      "AL"           TO   DSP-PRO
*             MOVE      0              TO   ERR-FLG
*    ELSE
*             MOVE      "NE"           TO   DSP-PRO
*    END-IF.
*
*    MOVE     WK-GRP         TO   DSP-GRP.
*    READ     DSPFILE.
*    MOVE     SPACE          TO   DSP-PRO.
*DSP-RD-RTN-EXIT.
*    EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL     ﾃﾞｨｽﾌﾟﾚｰ  WRITE                             *
*--------------------------------------------------------------*
*DSP-WRITE-RTN          SECTION.
*    MOVE     HEN-DATE       TO   SDATE.
*    MOVE     HEN-TIME       TO   STIME.
*    MOVE     HEN-TOKHAN-AREA     TO   TOKHAN.
*
*    MOVE     SPACE          TO   DSP-PRO.
*    WRITE    FZA00851.
*DSP-WRITE-EXIT.
*    EXIT.
*--------------------------------------------------------------*
*    LEVEL  ALL   入出庫ファイル ＲＥＡＤ
*--------------------------------------------------------------*
*NYU-READ-RTN       SECTION.
*    MOVE     ZERO      TO    NYU-FLG.
*    READ     NYSFILF   NEXT  AT  END
*             MOVE      1         TO   NYU-FLG
*             GO   TO   NYU-READ-EXIT
*    END-READ.
* 伝票_ブレイク
*    IF   DPNO     NOT =   NYU-F02
*         MOVE  1     TO   NYU-FLG
*         GO    TO    NYU-READ-EXIT
*    END-IF.
* 削除フラグ，または計上フラグが"１"の場合，読み飛ばし
*    IF   (NYU-F14      =   1)   OR
*         (NYU-F96      =   1)
*         GO  TO  NYU-READ-RTN
*    END-IF.
* ワークステイションが本社以外の時，自倉庫以外読み飛ばし
*    IF    WK-BSYNO1     =  "01"    OR
*          WK-BSYNO2     =  "01"
*          CONTINUE
*    ELSE
*          IF (NYU-F10   NOT =  WK-BSYNO1)   AND
*             (NYU-F11   NOT =  WK-BSYNO1)
*              GO  TO  NYU-READ-RTN
*          END-IF
*    END-IF.
*
*    IF   SRKBN        =   9
*         REWRITE  NYU-REC
*    END-IF.
*NYU-READ-EXIT.
*    EXIT.
*--------------------------------------------------------------*
*    LEVEL  ALL   入出庫ファイル ＲＥＡＤ１
*--------------------------------------------------------------*
*NYU-READ1-RTN       SECTION.
*    READ     NYSFILF   NEXT  AT  END
*             MOVE      1         TO   NYU-FLG
*             GO   TO   NYU-READ1-EXIT
*    END-READ.
* 削除フラグ，または計上フラグが"１"の場合，読み飛ばし
*    IF   (NYU-F14      =   1)   OR
*         (NYU-F96      =   1)
*         GO  TO  NYU-READ1-RTN
*    END-IF.
* ワークステイションが本社以外の時，自倉庫以外読み飛ばし
*    IF    WK-BSYNO1     =  "01"     OR
*          WK-BSYNO2     =  "01"
*          CONTINUE
*    ELSE
*          IF (NYU-F10   NOT =  WK-BSYNO1)   AND
*             (NYU-F11   NOT =  WK-BSYNO1)
*              GO  TO  NYU-READ1-RTN
*          END-IF
*    END-IF.
*
*    IF   SRKBN        =   9
*         REWRITE  NYU-REC
*    END-IF.
*NYU-READ1-EXIT.
*    EXIT.
*--------------------------------------------------------------*
*    LEVEL  ALL   商品在庫マスタ（変更前情報の更新）　　　　　 *
*--------------------------------------------------------------*
*ZAI-OLD-RTN        SECTION.
*出庫情報
*    IF    NYU-F10     NOT =  SPACE
*          MOVE  2                 TO   ZAI-FLG
*          MOVE  NYU-F10           TO   ZAI-F01
*          MOVE  NYU-F05           TO   ZAI-F021
*          MOVE  NYU-F06           TO   ZAI-F022
*          MOVE  NYU-F07           TO   ZAI-F03
*          *指定倉庫ＣＤ部門チェック
*          MOVE  NYU-F10           TO   WK-SOKCD
*          PERFORM HJYOKEN-READ-SEC
*                PERFORM    ZAI-READ-RTN
*                IF    INV-FLG    =  1
*                      PERFORM   ZAI-INIT-RTN
*                      PERFORM   ZAI-SUBTRACT-RTN
*                      MOVE      SYS-DATE2    TO   ZAI-F98
*                      WRITE     ZAI-REC
*                ELSE
*                      PERFORM   ZAI-SUBTRACT-RTN
*                      MOVE      SYS-DATE2    TO   ZAI-F99
*                      REWRITE   ZAI-REC
*                END-IF
*    END-IF.
*入庫情報
*    IF    NYU-F11     NOT =  SPACE
*          MOVE  1                 TO   ZAI-FLG
*          MOVE  NYU-F11           TO   ZAI-F01
*          MOVE  NYU-F05           TO   ZAI-F021
*          MOVE  NYU-F06           TO   ZAI-F022
*          MOVE  NYU-F08           TO   ZAI-F03
*          *指定倉庫ＣＤ部門チェック
*          MOVE  NYU-F11           TO   WK-SOKCD
*          PERFORM HJYOKEN-READ-SEC
*                PERFORM    ZAI-READ-RTN
*                IF    INV-FLG    =  1
*                      PERFORM   ZAI-INIT-RTN
*                      PERFORM   ZAI-SUBTRACT-RTN
*                      MOVE      SYS-DATE2    TO   ZAI-F98
*                      WRITE     ZAI-REC
*                ELSE
*                      PERFORM   ZAI-SUBTRACT-RTN
*                      MOVE      SYS-DATE2    TO   ZAI-F99
*                      REWRITE   ZAI-REC
*                END-IF
*    END-IF.
*ZAI-OLD-EXIT.
*    EXIT.
*--------------------------------------------------------------*
*    LEVEL  ALL   商品在庫マスタ（変更後情報の更新）　　　　　 *
*--------------------------------------------------------------*
 ZAI-NEW-RTN        SECTION.
*出庫情報
     IF    NYU-F10     NOT =  SPACE
           MOVE  2                 TO   ZAI-FLG
           MOVE  NYU-F10           TO   ZAI-F01
           MOVE  NYU-F05           TO   ZAI-F021
           MOVE  NYU-F06           TO   ZAI-F022
           MOVE  NYU-F07           TO   ZAI-F03
*          *指定倉庫ＣＤ部門チェック
           MOVE  NYU-F10           TO   WK-SOKCD
           PERFORM HJYOKEN-READ-SEC
                 PERFORM    ZAI-READ-RTN
                 IF    INV-FLG    =  1
                       PERFORM   ZAI-INIT-RTN
                       PERFORM   ZAI-ADD-RTN
                       MOVE      SYS-DATE2    TO   ZAI-F98
                       WRITE     ZAI-REC
                 ELSE
                       PERFORM   ZAI-ADD-RTN
                       MOVE      SYS-DATE2    TO   ZAI-F99
                       REWRITE   ZAI-REC
                 END-IF
     END-IF.
*入庫情報
     IF    NYU-F11     NOT =  SPACE
           MOVE  1                 TO   ZAI-FLG
           MOVE  NYU-F11           TO   ZAI-F01
           MOVE  NYU-F05           TO   ZAI-F021
           MOVE  NYU-F06           TO   ZAI-F022
           MOVE  NYU-F08           TO   ZAI-F03
*          *指定倉庫ＣＤ部門チェック
           MOVE  NYU-F11           TO   WK-SOKCD
           PERFORM HJYOKEN-READ-SEC
***********DISPLAY "WK-BUMON-CD  = " WK-BUMON-CD  UPON CONS
***********DISPLAY "HEN-BUMON-CD = " HEN-BUMON-CD UPON CONS
***********IF    WK-BUMON-CD = HEN-BUMON-CD
                 PERFORM    ZAI-READ-RTN
                 IF    INV-FLG    =  1
                       PERFORM   ZAI-INIT-RTN
                       PERFORM   ZAI-ADD-RTN
                       MOVE      SYS-DATE2    TO   ZAI-F98
                       WRITE     ZAI-REC
                 ELSE
                       PERFORM   ZAI-ADD-RTN
                       MOVE      SYS-DATE2    TO   ZAI-F99
                       REWRITE   ZAI-REC
                 END-IF
***********END-IF
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
           MOVE  NYU-F11           TO   ZAI-F01
           MOVE  NYU-F05           TO   ZAI-F021
           MOVE  NYU-F06           TO   ZAI-F022
           MOVE  NYU-F08           TO   ZAI-F03
     ELSE
           MOVE  NYU-F10           TO   ZAI-F01
           MOVE  NYU-F05           TO   ZAI-F021
           MOVE  NYU-F06           TO   ZAI-F022
           MOVE  NYU-F07           TO   ZAI-F03
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
*ZAI-SUBTRACT-RTN   SECTION.
*
*    IF    ZAI-FLG     =    2
*出庫情報
*          COMPUTE  ZAI-F04  =  ZAI-F04  +  NYU-F12
*          IF  NYU-F15  >  ZAI-SIME
*                COMPUTE  ZAI-F12  =  ZAI-F12  -  NYU-F12
*          ELSE
*                COMPUTE  ZAI-F06  =  ZAI-F06  +  NYU-F12
*                COMPUTE  ZAI-F08  =  ZAI-F08  -  NYU-F12
*          END-IF
*    ELSE
*入庫情報
*          COMPUTE  ZAI-F04  =  ZAI-F04  -  NYU-F12
*          IF  NYU-F15  >  ZAI-SIME
*                COMPUTE  ZAI-F11  =  ZAI-F11  -  NYU-F12
*          ELSE
*                COMPUTE  ZAI-F06  =  ZAI-F06  -  NYU-F12
*                COMPUTE  ZAI-F07  =  ZAI-F07  -  NYU-F12
*          END-IF
*    END-IF.
*ZAI-SUBTRACT-EXIT.
*    EXIT.
*--------------------------------------------------------------*
*    LEVEL  ALL   商品在庫・数量セット（変更後情報）
*--------------------------------------------------------------*
 ZAI-ADD-RTN        SECTION.
*
     IF    ZAI-FLG     =    2
*出庫情報
           COMPUTE  ZAI-F04  =  ZAI-F04  -  NYU-F12
           IF  NYU-F15  >  ZAI-SIME
                 COMPUTE  ZAI-F12  =  ZAI-F12  +  NYU-F12
           ELSE
                 COMPUTE  ZAI-F06  =  ZAI-F06  -  NYU-F12
                 COMPUTE  ZAI-F08  =  ZAI-F08  +  NYU-F12
           END-IF
     ELSE
*入庫情報
           COMPUTE  ZAI-F04  =  ZAI-F04  +  NYU-F12
           IF  NYU-F15  >  ZAI-SIME
                 COMPUTE  ZAI-F11  =  ZAI-F11  +  NYU-F12
           ELSE
                 COMPUTE  ZAI-F06  =  ZAI-F06  +  NYU-F12
                 COMPUTE  ZAI-F07  =  ZAI-F07  +  NYU-F12
           END-IF
     END-IF.
 ZAI-ADD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  ALL   商品在庫マスタ　存在チェック　　　　　　　　 *
*--------------------------------------------------------------*
 ZAI-CHK-SEC        SECTION.
     PERFORM  VARYING  IDX1  FROM  1  BY  1   UNTIL  IDX1  >  6
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
            IF    SGKBN  =  "I3"
               MOVE  BASY02        TO   ZAI-F01
               MOVE  SYOCD (IDX1)  TO   ZAI-F021
               MOVE  HIN101(IDX1)  TO   ZAI-F022(1:5)
               MOVE  HIN201(IDX1)  TO   ZAI-F022(6:2)
               MOVE  HIN301(IDX1)  TO   ZAI-F022(8:1)
               MOVE  AIT101(IDX1)  TO   ZAI-F03 (1:1)
               MOVE  AIT201(IDX1)  TO   ZAI-F03 (2:3)
               MOVE  AIT301(IDX1)  TO   ZAI-F03 (5:2)
               PERFORM   ZAI-READ-RTN
               IF   INV-FLG   =  1
                    MOVE   "R"  TO   EDIT-OPTION  OF  SYOCD (IDX1)
                    MOVE   "R"  TO   EDIT-OPTION  OF  HIN101(IDX1)
                    MOVE   "R"  TO   EDIT-OPTION  OF  HIN201(IDX1)
                    MOVE   "R"  TO   EDIT-OPTION  OF  HIN301(IDX1)
                    MOVE   "R"  TO   EDIT-OPTION  OF  AIT101(IDX1)
                    MOVE   "R"  TO   EDIT-OPTION  OF  AIT201(IDX1)
                    MOVE   "R"  TO   EDIT-OPTION  OF  AIT301(IDX1)
                    MOVE   18   TO   ERR-FLG
               ELSE
                    REWRITE   ZAI-REC
               END-IF
            END-IF
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
*SOK-READ-RTN         SECTION.
*    MOVE     0         TO   INV-FLG.
*    READ     ZSOKMS    INVALID   KEY
*             MOVE      1         TO   INV-FLG
*    END-READ.
*SOK-RD-EXIT.
*    EXIT.
*--------------------------------------------------------------*
*    LEVEL  ALL   作業区分マスタ　　ＲＥＡＤ　　　　　　　　　 *
*--------------------------------------------------------------*
*KBM-READ-RTN         SECTION.
*    MOVE     0         TO   INV-FLG.
*    READ     SGYKBMF   INVALID   KEY
*             MOVE      1         TO   INV-FLG
*    END-READ.
*KBM-RD-EXIT.
*    EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＨＥＡＤ　属性クリア　　　　　　　　　　　　*
*--------------------------------------------------------------*
*CLR-HEAD-RTN           SECTION.
*    MOVE     " "            TO   EDIT-CURSOR OF SRKBN
*                                 EDIT-CURSOR OF TANTO
*                                 EDIT-CURSOR OF DPKBN
*                                 EDIT-CURSOR OF SGKBN
*                                 EDIT-CURSOR OF DPNO
*                                 EDIT-CURSOR OF NYSYMD
*                                 EDIT-CURSOR OF BASY01
*                                 EDIT-CURSOR OF BASY02
*                                 EDIT-CURSOR OF BUCD1
*                                 EDIT-CURSOR OF BUCD2.
*    MOVE     "M"            TO   EDIT-OPTION OF SRKBN
*                                 EDIT-OPTION OF TANTO
*                                 EDIT-OPTION OF DPKBN
*                                 EDIT-OPTION OF SGKBN
*                                 EDIT-OPTION OF DPNO
*                                 EDIT-OPTION OF NYSYMD
*                                 EDIT-OPTION OF BASY01
*                                 EDIT-OPTION OF BASY02
*                                 EDIT-OPTION OF BUCD1
*                                 EDIT-OPTION OF BUCD2.
*
*    IF   WK-BSYNO1  =  "01"   OR
*         WK-BSYNO2  =  "01"
*         MOVE   " "         TO   EDIT-STATUS OF BASY01
*    ELSE
*         MOVE   "X"         TO   EDIT-STATUS OF BASY01
*    END-IF.
*CLR-HEAD-EXIT.
*    EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＢＯＤＹ　属性クリア　　　　　　　　　　　　*
*--------------------------------------------------------------*
*CLR-BODY-RTN           SECTION.
*    PERFORM  VARYING  I  FROM  1  BY  1  UNTIL  I  >  6
*      MOVE     " "      TO   EDIT-CURSOR OF DLKBN (I)
*                             EDIT-CURSOR OF TAN101(I)
*                             EDIT-CURSOR OF TAN201(I)
*                             EDIT-CURSOR OF TAN301(I)
*                             EDIT-CURSOR OF SYOCD (I)
*                             EDIT-CURSOR OF HIN101(I)
*                             EDIT-CURSOR OF HIN201(I)
*                             EDIT-CURSOR OF HIN301(I)
*                             EDIT-CURSOR OF SURYOU(I)
*                             EDIT-CURSOR OF STK101(I)
*                             EDIT-CURSOR OF STK201(I)
*                             EDIT-CURSOR OF BIKOU (I)
*                             EDIT-CURSOR OF AIT101(I)
*                             EDIT-CURSOR OF AIT201(I)
*                             EDIT-CURSOR OF AIT301(I)
*      MOVE     "M"      TO   EDIT-OPTION OF DLKBN (I)
*                             EDIT-OPTION OF TAN101(I)
*                             EDIT-OPTION OF TAN201(I)
*                             EDIT-OPTION OF TAN301(I)
*                             EDIT-OPTION OF SYOCD (I)
*                             EDIT-OPTION OF HIN101(I)
*                             EDIT-OPTION OF HIN201(I)
*                             EDIT-OPTION OF HIN301(I)
*                             EDIT-OPTION OF SURYOU(I)
*                             EDIT-OPTION OF STK101(I)
*                             EDIT-OPTION OF STK201(I)
*                             EDIT-OPTION OF BIKOU (I)
*                             EDIT-OPTION OF AIT101(I)
*                             EDIT-OPTION OF AIT201(I)
*                             EDIT-OPTION OF AIT301(I)
*    END-PERFORM.
*CLR-BODY-EXIT.
*    EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＴＡＩＬ　属性クリア　　　　　　　　　　　　*
*--------------------------------------------------------------*
*CLR-TAIL-RTN           SECTION.
*    MOVE     " "        TO   EDIT-CURSOR OF KAKNIN.
*    MOVE     "M"        TO   EDIT-OPTION OF KAKNIN.
*CLR-TAIL-EXIT.
*    EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      プロテクトセット（明細行）　　　　　　　　　*
*--------------------------------------------------------------*
*PRO-BODY-RTN           SECTION.
*未入力行にプロテクトセット
*    MOVE  "X"           TO   EDIT-STATUS  OF  TAN101(I)
*                             EDIT-STATUS  OF  TAN201(I)
*                             EDIT-STATUS  OF  TAN301(I)
*                             EDIT-STATUS  OF  SYOCD (I)
*                             EDIT-STATUS  OF  HIN101(I)
*                             EDIT-STATUS  OF  HIN201(I)
*                             EDIT-STATUS  OF  HIN301(I)
*                             EDIT-STATUS  OF  SURYOU(I)
*                             EDIT-STATUS  OF  STK101(I)
*                             EDIT-STATUS  OF  STK201(I)
*                             EDIT-STATUS  OF  BIKOU (I)
*                             EDIT-STATUS  OF  HINMEI(I)
*                             EDIT-STATUS  OF  AIT101(I)
*                             EDIT-STATUS  OF  AIT201(I)
*                             EDIT-STATUS  OF  AIT301(I)
*                             EDIT-STATUS  OF  DLKBN (I).
*PRO-BODY-EXIT.
*    EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      プロテクトセット（明細項目）　　　　　　　　*
*--------------------------------------------------------------*
*PRO-SET-RTN            SECTION.
*    PERFORM  VARYING  I  FROM  1  BY  1  UNTIL  I  >  6
*      IF   ( SGKBN   NOT =  "I3" )   OR
*           ( SRKBN   =   2   AND   DEL-FLG (I) =  1 )
*             MOVE "X"   TO   EDIT-STATUS OF AIT101(I)
*                             EDIT-STATUS OF AIT201(I)
*                             EDIT-STATUS OF AIT301(I)
*      ELSE
*             MOVE " "   TO   EDIT-STATUS OF AIT101(I)
*                             EDIT-STATUS OF AIT201(I)
*                             EDIT-STATUS OF AIT301(I)
*      END-IF
*      IF   ( SRKBN   =   1 )    OR
*           ( SRKBN   =   2   AND   DEL-FLG (I) =  1 )
*             MOVE "X"   TO   EDIT-STATUS OF DLKBN (I)
*      ELSE
*             MOVE " "   TO   EDIT-STATUS OF DLKBN (I)
*      END-IF
*    END-PERFORM.
*PRO-SET-EXIT.
*    EXIT.
*--------------------------------------------------------------*
*    条件マスタ読込み
*--------------------------------------------------------------*
 HJYOKEN-READ-SEC       SECTION.
*特販部名称編集
     MOVE     SPACE               TO   JYO-REC.
     INITIALIZE                        JYO-REC.
     MOVE    "20"                 TO   JYO-F01.
     MOVE    WK-SOKCD             TO   JYO-F02.
     READ     HJYOKEN
       INVALID KEY
              DISPLAY NC"＃＃条件Ｆ異常＃＃" UPON  CONS
              DISPLAY NC"＃＃部門ＣＨＫ＃＃" UPON  CONS
              STOP  RUN
       NOT INVALID KEY
              MOVE SPACE          TO   HJYOKEN-INV-FLG
              MOVE JYO-F05        TO   WK-BUMON-CD
     END-READ.
 HJYOKEN-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*
