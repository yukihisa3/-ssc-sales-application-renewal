****************************************************************
*                                                              *
*    顧客名　　　　　　　：　サカタのタネ（株）殿　　　　　　　*
*    業務名　　　　　　　：　出荷管理システム　　　　　　　　　*
*    モジュール名　　　　：　量販店データ伝票_採番　　　　　　*
*    作成日／作成者　　　：　99/10/06  HAGIWARA                *
*    再利用ＰＧ　　　　　：  SSKT055.SKTSLIB                   *
*    更新日／更新者　　　：　2000/04/18 NAV TAKAHASHI          *
*                    備考マスタからの備考レコード作成          *
*    更新日／更新者　　　：　2008/08/08 NAV TAKAHASHI          *
*                    内部統制対応　　　　　　　　　　          *
*    更新日／更新者　　　：　2012/11/07 NAV TAKAHASHI          *
*                    登録時の数量、単価、金額を登録エリアセット*
****************************************************************
*<履歴>*********************************************************
* XXXX/XX/XX XXXXXXXXX ＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮ
* 2018/04/02 高橋　　　受注残ファイル更新処理を追加
* 2020/04/27 高橋　　　Ｄ３６５伝票番号取得、小売連携区分取得　
* 2020/12/16 高橋　　　小売連携区分取得方法変更
****************************************************************
 IDENTIFICATION              DIVISION.
 PROGRAM-ID.                 TSY0604B.
 ENVIRONMENT                 DIVISION.
 CONFIGURATION               SECTION.
 SOURCE-COMPUTER.
 OBJECT-COMPUTER.
 SPECIAL-NAMES.
     YA            IS        CHR-2
     YB-21         IS        CHR-21
     YB            IS        CHR-15
     CONSOLE       IS        CONS
     STATION       IS        STAT.
****************************************************************
 INPUT-OUTPUT              SECTION.
****************************************************************
 FILE-CONTROL.
*伝票データＦ２
     SELECT      SHTDENWK    ASSIGN    TO        DA-01-S-SHTDENWK
                             FILE      STATUS    DEN2-ST.
*伝票データＦ
*    SELECT      SHTDENF     ASSIGN    TO        DA-01-S-SHTDENF
     SELECT      SHTDENF     ASSIGN    TO        SHTDENF
                             ORGANIZATION        SEQUENTIAL
                             ACCESS    MODE      SEQUENTIAL
                             FILE      STATUS    DEN-ST.
*取引先マスタ
     SELECT      HTOKMS      ASSIGN    TO        DA-01-VI-TOKMS2
                             ORGANIZATION        INDEXED
                             ACCESS    MODE      RANDOM
                             RECORD    KEY       TOK-F01
                             FILE      STATUS    TOK-ST.
*プリント定義ファイル
     SELECT     PRTFILE      ASSIGN    TO        LP-04-PRTF
                             FILE      STATUS    PRT-ST.
*商品在庫マスタ
     SELECT     ZAMZAIF      ASSIGN    TO        DA-01-VI-ZAMZAIL1
                             ORGANIZATION        INDEXED
                             ACCESS    MODE      RANDOM
                             RECORD    KEY       ZAI-F01
                                                 ZAI-F02
                                                 ZAI-F03
                             FILE      STATUS    ZAI-ST.
*商品コード変換テーブル
     SELECT     HSHOTBL      ASSIGN    TO        DA-01-VI-SHOTBL1
                             ORGANIZATION        INDEXED
                             ACCESS    MODE      RANDOM
                             RECORD    KEY       SHO-F01
                                                 SHO-F02
                             FILE      STATUS    SHO-ST.
*商品名称マスタ
     SELECT     HMEIMS       ASSIGN    TO        DA-01-VI-MEIMS1
                             ORGANIZATION        INDEXED
                             ACCESS    MODE      RANDOM
                             RECORD    KEY       MEI-F01
                             FILE      STATUS    MEI-ST.
*量販店備考ワーク
     SELECT     SHTBIKF      ASSIGN    TO        DA-01-VI-SHTBIKL1
                             ORGANIZATION        INDEXED
                             ACCESS    MODE      RANDOM
                             RECORD    KEY       BIK-F01
                                                 BIK-F02
                                                 BIK-F03
                             FILE      STATUS    BIK-ST.
*量販店データ変換リストワーク
     SELECT     DENMEISF     ASSIGN    TO        DA-01-S-DENMEISF
                             FILE      STATUS    DME-ST.
*担当者マスタ
     SELECT     HTANMS       ASSIGN    TO        DA-01-VI-TANMS1
                             ORGANIZATION        INDEXED
                             ACCESS    MODE      RANDOM
                             RECORD    KEY       TAN-F01  TAN-F02
                             FILE      STATUS    TAN-ST.
*# 2020/04/27 NAV ST Ｄ３６５連携対応
*商品コード変換テーブル
     SELECT     SUBTBLF      ASSIGN    TO        DA-01-VI-SUBTBLL1
                             ORGANIZATION        INDEXED
                             ACCESS    MODE      RANDOM
                             RECORD    KEY       SUB-F01
                                                 SUB-F02
                             FILE      STATUS    SUB-ST.
*# 2020/04/27 NAV ED Ｄ３６５連携対応
****************************************************************
 DATA                        DIVISION.
****************************************************************
 FILE                        SECTION.
*伝票データＦ
 FD  SHTDENWK
     LABEL       RECORD      IS        STANDARD
     BLOCK       CONTAINS    1         RECORDS.
     COPY        SHTDENWK    OF        XFDLIB
     JOINING     DEN2        AS        PREFIX.
*伝票データＦ
 FD  SHTDENF
     LABEL       RECORD      IS        STANDARD
     BLOCK       CONTAINS    1         RECORDS.
     COPY        SHTDENF     OF        XFDLIB
     JOINING     DEN         AS        PREFIX.
*取引先マスタ
 FD  HTOKMS
     LABEL       RECORD      IS        STANDARD
     BLOCK       CONTAINS     8        RECORDS.
     COPY        HTOKMS      OF        XFDLIB
     JOINING     TOK         AS        PREFIX.
*プリントファイル
 FD  PRTFILE
     LABEL       RECORD    IS        OMITTED.
 01  PRT-REC.
     03  FILLER            PIC X(200).
*商品在庫マスタ
 FD  ZAMZAIF.
     COPY     ZAMZAIF   OF        XFDLIB
              JOINING   ZAI       PREFIX.
*商品変換テーブル
 FD  HSHOTBL.
     COPY     HSHOTBL   OF        XFDLIB
              JOINING   SHO       PREFIX.
*商品名称マスタ
 FD  HMEIMS.
     COPY     HMEIMS    OF        XFDLIB
              JOINING   MEI       PREFIX.
*量販店備考ワーク
 FD  SHTBIKF.
     COPY     SHTDENWK  OF        XFDLIB
              JOINING   BIK       PREFIX.
*伝票データＦ
 FD  DENMEISF
     LABEL       RECORD      IS        STANDARD
     BLOCK       CONTAINS    1         RECORDS.
     COPY        SHTDENWK    OF        XFDLIB
     JOINING     DME         AS        PREFIX.
*担当者マスタ
 FD  HTANMS.
     COPY     HTANMS    OF        XFDLIB
              JOINING   TAN       PREFIX.
*# 2020/04/27 NAV ST Ｄ３６５連携対応
*ＳＵＢ商品変換テーブル
 FD  SUBTBLF.
     COPY     SUBTBLF   OF        XFDLIB
              JOINING   SUB       PREFIX.
*# 2020/04/27 NAV ED Ｄ３６５連携対応
****************************************************************
 WORKING-STORAGE           SECTION.
****************************************************************
 01  ST-AREA.
     03  IN-DATA             PIC  X(01)  VALUE  SPACE.
     03  DEN2-ST             PIC  X(02)  VALUE  SPACE.
     03  DEN-ST              PIC  X(02)  VALUE  SPACE.
     03  TOK-ST              PIC  X(02)  VALUE  SPACE.
     03  PRT-ST              PIC  X(02)  VALUE  SPACE.
     03  ZAI-ST              PIC  X(02)  VALUE  SPACE.
     03  SHO-ST              PIC  X(02)  VALUE  SPACE.
     03  MEI-ST              PIC  X(02)  VALUE  SPACE.
     03  BIK-ST              PIC  X(02)  VALUE  SPACE.
     03  DME-ST              PIC  X(02)  VALUE  SPACE.
     03  TAN-ST              PIC  X(02)  VALUE  SPACE.
*# 2020/04/27 NAV ST Ｄ３６５連携対応
     03  SUB-ST              PIC  X(02)  VALUE  SPACE.
*# 2020/04/27 NAV ED Ｄ３６５連携対応
 01  WK-AREA.
     03  END-FLG             PIC  9(01)  VALUE  ZERO.
     03  I                   PIC  9(01)  VALUE  ZERO.
     03  INV-SW              PIC  9(01)  VALUE  ZERO.
     03  WK-DENCNT           PIC  9(09)  VALUE  ZERO.
     03  IX                  PIC  9(01)  VALUE  ZERO.
     03  WK-TOKCD            PIC  9(08)  VALUE  ZERO.
     03  WK-TENCD            PIC  9(05)  VALUE  ZERO.
     03  WK-DENNO            PIC  9(09)  VALUE  ZERO.
*\\\ 93.06.04 START \\\
     03  WK-SYUKA            PIC  9(08)  VALUE  ZERO.
     03  WK-BASYO            PIC  X(02)  VALUE  SPACE.
*\\\ 93.06.04 END   \\\
     03  WK-TOKNM            PIC  N(10).
     03  L-CNT               PIC  99     VALUE  ZERO.
     03  P-CNT               PIC  99     VALUE  ZERO.
     03  KEP-FLG             PIC  X(01)  VALUE  SPACE.
     03  HSHOTBL-INV-FLG     PIC  X(03)  VALUE  SPACE.
     03  HMEIMS-INV-FLG      PIC  X(03)  VALUE  SPACE.
*# 2020/04/27 NAV ST Ｄ３６５連携対応
     03  SUBTBLF-INV-FLG     PIC  X(03)  VALUE  SPACE.
*# 2020/04/27 NAV ED Ｄ３６５連携対応
     03  BIKO-INV-FLG        PIC  X(03)  VALUE  SPACE.
     03  WK-DENBIKO          PIC  9(09)  VALUE  ZERO.
     03  WK-DENPYO           PIC  9(09)  VALUE  ZERO.
     03  WK-TORBIKO          PIC  9(08)  VALUE  ZERO.
     03  TAN-INV-FLG         PIC  X(03)  VALUE  SPACE.
*計算領域
 01  WRK-AREA.
     03  WRK-HIK             PIC S9(09)V9(02)  VALUE ZERO.
     03  WRK-ZAI             PIC S9(09)V9(02)  VALUE ZERO.
*
 01  DENNO-S                 PIC  9(09)  VALUE  999999999.
 01  DENNO-L                 PIC  9(09)  VALUE  ZERO.
 01  WK-DENBK                PIC  9(09)  VALUE  ZERO.
*日付取得
 01  SYS-DATE                PIC  9(06)  VALUE  ZERO.
 01  WK-DATE8.
     03  WK-Y                PIC  9(04)  VALUE  ZERO.
     03  WK-M                PIC  9(02)  VALUE  ZERO.
     03  WK-D                PIC  9(02)  VALUE  ZERO.
***  エラーセクション名
 01  SEC-NAME.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-SEC    => ".
     03  S-NAME                   PIC  X(20).
*
*---<< ｻﾌﾞﾙｰﾁﾝ LINK AREA >>-*
 01  LINK-AREA.
     03  LINK-IN.
         05  LI-KBN          PIC  9(01).
         05  LI-KETA         PIC  9(01).
         05  LI-START        PIC  9(09).
         05  LI-END          PIC  9(09).
         05  LI-DENNO        PIC  9(09).
     03  LINK-OUT.
         05  LO-ERR          PIC  9(01).
         05  LO-NEXT         PIC  9(09).
*
 01  FILE-ERR.
     03  DEN2-ERR            PIC  N(10)  VALUE
                   NC"伝票データＦ２異常".
     03  DEN-ERR             PIC  N(10)  VALUE
                   NC"伝票データＦ異常".
     03  TOK-ERR             PIC  N(10)  VALUE
                   NC"取引先マスタ異常".
     03  PRT-ERR             PIC  N(10)  VALUE
                   NC"プリントＦ異常".
     03  ZAI-ERR             PIC  N(10)  VALUE
                   NC"在庫マスタ異常".
     03  SHO-ERR             PIC  N(10)  VALUE
                   NC"商品変換テーブル異常".
     03  MEI-ERR             PIC  N(10)  VALUE
                   NC"商品名称マスタ異常".
     03  BIK-ERR             PIC  N(10)  VALUE
                   NC"量販店備考ワーク異常".
     03  DME-ERR             PIC  N(10)  VALUE
                   NC"量販店変換リスト異常".
     03  TAN-ERR             PIC  N(10)  VALUE
                   NC"担当者マスタ　　異常".
*# 2020/04/27 NAV ST Ｄ３６５連携対応
     03  SUB-ERR             PIC  N(12)  VALUE
                   NC"ＳＵＢ商品変換ＴＢＬ異常".
*# 2020/04/27 NAV ED Ｄ３６５連携対応
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN             PIC X(01).
 01  LINK-IN-YMD6            PIC 9(06).
 01  LINK-IN-YMD8            PIC 9(08).
 01  LINK-OUT-RET            PIC X(01).
 01  LINK-OUT-YMD            PIC 9(08).
*
 01  WK-TOUSEI.
     03  WK-DEN-F59          PIC  X(02)  VALUE  SPACE.
     03  WK-DEN-F60          PIC  X(02)  VALUE  SPACE.
     03  WK-DEN-F61          PIC  X(02)  VALUE  SPACE.
     03  WK-DEN-F62          PIC  9(08)  VALUE  ZERO.
     03  WK-DEN-F63          PIC  9(08)  VALUE  ZERO.
     03  WK-DEN-F64          PIC  9(08)  VALUE  ZERO.
     03  WK-DEN-F65          PIC  X(01)  VALUE  SPACE.
     03  WK-DEN-F66          PIC  X(01)  VALUE  SPACE.
****************************************************************
*    プリントエリア                                            *
****************************************************************
*--------------------------------------------------------------*
*    ヘッダ                                                    *
*--------------------------------------------------------------*
*
 01  HD1.
     03  FILLER                  PIC  X(40)  VALUE  SPACE.
     03  FILLER                  PIC  N(17)  VALUE
         NC"※※　量販店伝票_採番リスト　※※"
                                 CHARACTER  TYPE  IS  CHR-21.
     03  FILLER                  PIC  X(13)  VALUE  SPACE.
     03  HD1-01                  PIC  9(04).
     03  FILLER                  PIC  N(01)  VALUE  NC"年"
                                 CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  HD1-02                  PIC  Z9.
     03  FILLER                  PIC  N(01)  VALUE  NC"月"
                                 CHARACTER  TYPE  IS  CHR-2.
     03  HD1-03                  PIC  Z9.
     03  FILLER                  PIC  N(01)  VALUE  NC"日"
                                 CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(03)  VALUE  SPACE.
     03  HD1-04                  PIC  ZZ9.
     03  FILLER                  PIC  N(01)  VALUE  NC"頁"
                                 CHARACTER  TYPE  IS  CHR-2.
*
 01  HD2.
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  FILLER                  PIC  N(04)  VALUE NC"端末名："
                                 CHARACTER  TYPE  IS  CHR-2.
     03  HD2-01                  PIC  X(08)  VALUE  SPACE.
     03  FILLER                  PIC  X(22)  VALUE  SPACE.
*
 01  HD3.
     03  FILLER                  PIC  X(35)  VALUE  SPACE.
     03  FILLER                  PIC  N(03)  VALUE
                                 NC"取引先"
                                 CHARACTER   TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(35)  VALUE  SPACE.
     03  FILLER                  PIC  N(05)  VALUE
                                 NC"開始伝票_"
                                 CHARACTER   TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(05)  VALUE  SPACE.
     03  FILLER                  PIC  N(05)  VALUE
                                 NC"終了伝票_"
                                 CHARACTER  TYPE  IS  CHR-2.
*
 01  SEN                         CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  N(25)  VALUE
         NC"─────────────────────────".
     03  FILLER                  PIC  N(25)  VALUE
         NC"─────────────────────────".
     03  FILLER                  PIC  N(18)  VALUE
         NC"──────────────────".
*01  SEN1.
*    03  FILLER                  PIC  X(50)  VALUE
*        "--------------------------------------------------".
*    03  FILLER                  PIC  X(50)  VALUE
*        "--------------------------------------------------".
*    03  FILLER                  PIC  X(36)  VALUE
*        "------------------------------------".
 01  DT1                         CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(35)  VALUE  SPACE.
     03  DT1-01                  PIC  9(08).
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  DT1-02                  PIC  N(10).
     03  FILLER                  PIC  X(11)  VALUE  SPACE.
     03  DT1-03                  PIC  9(09).
     03  FILLER                  PIC  X(06)  VALUE  SPACE.
     03  DT1-04                  PIC  9(09).
*#2018/04/02 NAV ST
*受注残ファイル更新用ワーク
 01  PARA-AREA.
     03  PARA-KOSINKBN         PIC X(01).
     03  PARA-DTKBN            PIC X(01).
     03  PARA-TOKCD            PIC 9(08).
     03  PARA-DENNO            PIC 9(09).
     03  PARA-GYO              PIC 9(02).
     03  PARA-SOUSAI           PIC 9(01).
     03  PARA-DENKU            PIC 9(02).
     03  PARA-TENCD            PIC 9(05).
     03  PARA-SOKCD            PIC X(02).
     03  PARA-HATYU            PIC 9(08).
     03  PARA-NOUHIN           PIC 9(08).
     03  PARA-SYUKA            PIC 9(08).
     03  PARA-SAKATACD         PIC X(08).
     03  PARA-HINTAN1          PIC X(05).
     03  PARA-HINTAN2          PIC X(02).
     03  PARA-HINTAN3          PIC X(01).
     03  PARA-TANABAN          PIC X(06).
     03  PARA-JYUTYUSU         PIC 9(09).
     03  PARA-SYUKASU          PIC 9(09).
     03  PARA-GENKA            PIC 9(09).
     03  PARA-BAIKA            PIC 9(09).
     03  PARA-GENKAKIN         PIC 9(09).
     03  PARA-BAIKAKIN         PIC 9(09).
     03  PARA-AITECD           PIC X(13).
     03  PARA-HIKIATE          PIC X(01).
     03  PARA-UPBUMON          PIC X(04).
     03  PARA-UPTANCD          PIC X(02).
     03  PARA-JDATE            PIC 9(08).
     03  PARA-JTIME            PIC 9(04).
   01  PARA-BUMON              PIC X(04).
   01  PARA-TANCD              PIC X(02).
*#2018/04/02 NAV ED
*#2018/04/02 NAV ST
 01  WK-TANABAN                  PIC  X(06)  VALUE  SPACE.
*#2018/04/02 NAV ED
*#2020/04/16 NAV ST Ｄ３６５伝票番号取得
 01  WK-D365-DEN-PARA.
     03  WK-D365-PARA-IN1   PIC   X(01).
     03  WK-D365-PARA-IN2   PIC   9(08).
     03  WK-D365-PARA-OUT1  PIC   X(20).
     03  WK-D365-PARA-OUT2  PIC   X(01).
*#2020/04/16 NAV ED Ｄ３６５伝票番号取得
****************************************************************
 LINKAGE                     SECTION.
****************************************************************
 01  LINK-WKSTNNM                PIC  X(08).
 01  LINK-BUMON                  PIC  X(04).
 01  LINK-TANCD                  PIC  X(02).
****************************************************************
 PROCEDURE                   DIVISION  USING LINK-WKSTNNM
                                             LINK-BUMON
                                             LINK-TANCD.
****************************************************************
 DECLARATIVES.
 DEN2-ERR                    SECTION.
     USE         AFTER       EXCEPTION PROCEDURE SHTDENWK.
     DISPLAY     DEN2-ERR    UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     DEN2-ST     UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     ACCEPT      IN-DATA     FROM      CONS.
     STOP        RUN.
 DEN-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE SHTDENF.
     DISPLAY     DEN-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     DEN-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     ACCEPT      IN-DATA     FROM      CONS.
     STOP        RUN.
 TOK-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE HTOKMS.
     DISPLAY     TOK-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     TOK-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     ACCEPT      IN-DATA     FROM      CONS.
     STOP        RUN.
 PRT-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE PRTFILE.
     DISPLAY     PRT-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     PRT-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     ACCEPT      IN-DATA     FROM      CONS.
     STOP        RUN.
 ZAI-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE ZAMZAIF.
     DISPLAY     ZAI-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     ZAI-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     ACCEPT      IN-DATA     FROM      CONS.
     STOP        RUN.
 SHO-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE HSHOTBL.
     DISPLAY     SHO-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     SHO-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     ACCEPT      IN-DATA     FROM      CONS.
     STOP        RUN.
 MEI-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE HMEIMS.
     DISPLAY     MEI-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     MEI-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     ACCEPT      IN-DATA     FROM      CONS.
     STOP        RUN.
 BIK-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE SHTBIKF.
     DISPLAY     BIK-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     BIK-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     ACCEPT      IN-DATA     FROM      CONS.
     STOP        RUN.
 DME-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE DENMEISF.
     DISPLAY     DME-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     DME-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     ACCEPT      IN-DATA     FROM      CONS.
     STOP        RUN.
 TAN-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE HTANMS.
     DISPLAY     TAN-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     TAN-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     ACCEPT      IN-DATA     FROM      CONS.
     STOP        RUN.
*# 2020/04/27 NAV ST Ｄ３６５連携対応
 TBL-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE SUBTBLF.
     DISPLAY     SUB-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     SUB-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     ACCEPT      IN-DATA     FROM      CONS.
     STOP        RUN.
*# 2020/04/27 NAV ED Ｄ３６５連携対応
 END DECLARATIVES.
****************************************************************
*                 P R O G R A M - S E C
****************************************************************
 PROGRAM-SEC                 SECTION.
     PERFORM     INIT-SEC.
     PERFORM     MAIN-SEC    UNTIL     END-FLG  = 9.
     PERFORM     END-SEC.
     STOP        RUN.
*PROGRAM-END.
****************************************************************
*                 I N I T - S E C
****************************************************************
 INIT-SEC                    SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
*
     OPEN        I-O         SHTDENWK  HTOKMS  ZAMZAIF
                 INPUT       HSHOTBL   HMEIMS  SHTBIKF  HTANMS
*# 2020/04/27 NAV ST Ｄ３６５連携対応
                             SUBTBLF
*# 2020/04/27 NAV ED Ｄ３６５連携対応
                 EXTEND      SHTDENF
                 OUTPUT      PRTFILE   DENMEISF.
*システム日付・時刻の取得
     ACCEPT   SYS-DATE          FROM   DATE.
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
     MOVE      LINK-OUT-YMD       TO   WK-DATE8.
*パラ担当者で承認権限取得（内部統制更新項目）
     PERFORM   TAN-READ-SEC.
*量販ＲＥＣ初期化
     MOVE        SPACE       TO   DEN-REC.
     INITIALIZE  DEN-REC.
 INIT-010.
*伝票データ２初期読み
     PERFORM  DEN2-RD-SEC.
     IF    END-FLG  =  0
       MOVE     DEN2-F01       TO   WK-TOKCD
       MOVE     DEN2-F02       TO   WK-DENNO
       MOVE     DEN2-F07       TO   WK-TENCD
*\\\ 93.06.04 START \\\
       MOVE     DEN2-F112      TO   WK-SYUKA
       MOVE     DEN2-F08       TO   WK-BASYO
*\\\ 93.06.04 END   \\\
*取引先マスタＲＥＡＤ
       PERFORM  TOK-RD-SEC
       IF   INV-SW  =  1
              GO        TO        INIT-010
       END-IF
     END-IF.
     PERFORM    HEAD-WT-SEC.
 INIT-EXIT.
     EXIT.
****************************************************************
*                 M A I N - S E C
****************************************************************
 MAIN-SEC                    SECTION.
     MOVE     "MAIN-SEC"          TO   S-NAME.
*
     IF  L-CNT    >  60
*         改頁
          MOVE          SPACE     TO        PRT-REC
          WRITE   PRT-REC  AFTER  PAGE
          MOVE    ZERO     TO     L-CNT
*
          PERFORM  HEAD-WT-SEC
     END-IF.
*
*取引先コードがブレイク
     IF  DEN2-F01 NOT = WK-TOKCD
         PERFORM   BODY-WT-SEC
*********2012/03/23 ST 伝票番号を最後に必ずカウントアップする
         PERFORM   DEN-GET-SEC
*********2012/03/23 END
         PERFORM   TOK-WT-SEC
         MOVE      DEN2-F01       TO   WK-TOKCD
         MOVE      DEN2-F02       TO   WK-DENNO
         MOVE      DEN2-F07       TO   WK-TENCD
         MOVE      DEN2-F112      TO   WK-SYUKA
         MOVE      DEN2-F08       TO   WK-BASYO
         MOVE      999999999      TO   DENNO-S
         MOVE      ZERO           TO   DENNO-L
         PERFORM   BIKOU-ADD-SEC
         PERFORM   TOK-RD-SEC
         IF   (INV-SW  =  1)
           GO             TO        DEN2-READ
         END-IF
     END-IF.
*
*指定伝票_又は店舗コードがブレイク
     IF  DEN2-F02 NOT = WK-DENNO
     OR  DEN2-F07 NOT = WK-TENCD
     OR  DEN2-F112 NOT = WK-SYUKA
     OR  DEN2-F08  NOT = WK-BASYO
         PERFORM   BIKOU-ADD-SEC
         PERFORM   DEN-GET-SEC
         MOVE      0              TO   IX
         MOVE      DEN2-F02       TO   WK-DENNO
         MOVE      DEN2-F07       TO   WK-TENCD
         MOVE      DEN2-F112      TO   WK-SYUKA
         MOVE      DEN2-F08       TO   WK-BASYO
     END-IF.
*
*行_は６まで
     ADD   1       TO     IX.
     IF    IX      >   6
         PERFORM   BIKOU-ADD-SEC
         PERFORM   DEN-GET-SEC
         MOVE      1              TO   IX
     END-IF.
*伝票データ作成
     MOVE     DEN2-REC       TO        DEN-REC.
 MAIN061.
     MOVE     WK-DENCNT      TO        DEN-F02.
     MOVE     WK-DENCNT      TO        DEN-F23.
*# 2020/04/27 NAV ST Ｄ３６５対応
     MOVE     WK-D365-PARA-OUT1   TO   DEN-D99.
*# 2020/04/27 NAV ED Ｄ３６５対応
     MOVE     ZERO           TO        DEN-F113.
     MOVE     IX             TO        DEN-F03.
*****内部統制対応
     MOVE     WK-DEN-F59     TO        DEN-F59.
     MOVE     WK-DEN-F60     TO        DEN-F60.
     MOVE     WK-DEN-F61     TO        DEN-F61.
     MOVE     WK-DEN-F62     TO        DEN-F62.
     MOVE     WK-DEN-F63     TO        DEN-F63.
     MOVE     WK-DEN-F64     TO        DEN-F64.
     MOVE     WK-DEN-F65     TO        DEN-F65.
     MOVE     WK-DEN-F66     TO        DEN-F66.
     MOVE     ZERO           TO        DEN-F67.
     MOVE     LINK-TANCD     TO        DEN-F06.
*
       IF       DEN-F02        <         DENNO-S
                MOVE    DEN-F02   TO     DENNO-S
       END-IF.
       IF       DEN-F02        >         DENNO-L
                MOVE    DEN-F02   TO     DENNO-L
       END-IF.
*
 MAIN062.
*商品変換ＴＢＬチェック*
     PERFORM     HSHOTBL-READ-SEC.
     IF  HSHOTBL-INV-FLG  =  SPACE
*        *商品名称マスタチェック*
         MOVE    SHO-F031  TO        MEI-F011
         MOVE    SHO-F032  TO        MEI-F012
         PERFORM  HMEIMS-READ-SEC
     END-IF.
*#   2012/07/27↓小売連携区分セット
*TTT2020/12/16 NAV ST
*TTT IF  HMEIMS-INV-FLG = SPACE
*TTT     MOVE    MEI-F10   TO        DEN-F32
*# 2020/04/27 NAV ST Ｄ３６５連携対応
*TTT     IF   MEI-F10  =  SPACE
*TTT          PERFORM  SUBTBLF-READ-SEC
*TTT          IF  SUBTBLF-INV-FLG  =  SPACE
*TTT              IF  SUB-F19  =  "1"
*TTT                  MOVE  "2"      TO    DEN-F32
*TTT              ELSE
*TTT                  IF  SUB-F19  =  "2"
*TTT                      MOVE "3"   TO    DEN-F32
*TTT                  ELSE
*TTT                      MOVE SPACE TO    DEN-F32
*TTT                  END-IF
*TTT              END-IF
*TTT         ELSE
*TTT              MOVE    SPACE      TO    DEN-F32
*TTT          END-IF
*TTT    END-IF
*TTT2020/12/16 NAV ED
*TTT2020/12/16 NAV ST 条件追加
     PERFORM  SUBTBLF-READ-SEC.
     IF  SUBTBLF-INV-FLG  =  SPACE
         MOVE   SUB-F19          TO    DEN-F32
     ELSE
         MOVE   SPACE            TO    DEN-F32
     END-IF.
*# 2020/04/27 NAV ED Ｄ３６５連携対応
*TT2020/12/16 NAV ST
*TTT ELSE
*TTT     MOVE    SPACE     TO        DEN-F32
*TTT     PERFORM  SUBTBLF-READ-SEC
*TTT     IF  SUBTBLF-INV-FLG  =  SPACE
*********AND SUB-F19  NOT =  SPACE
*TTT         IF  SUB-F19  =  "1"
*TTT             MOVE  "2"      TO    DEN-F32
*TTT         ELSE
*TTT             IF  SUB-F19  =  "2"
*TTT                 MOVE "3"   TO    DEN-F32
*TTT             ELSE
*TTT                 MOVE SPACE TO    DEN-F32
*TTT             END-IF
*TTT         END-IF
*TTT     ELSE
*TTT         MOVE    SPACE      TO    DEN-F32
*TTT     END-IF
*TTT END-IF.
*TT2020/12/16 NAV ED
*2009.09.14
     IF     SHO-F09   =  ZERO
         MOVE    MEI-F041  TO        DEN-F171
     ELSE
         MOVE    SHO-F09   TO        DEN-F171
     END-IF.
*在庫引当
     IF  HSHOTBL-INV-FLG  =  SPACE
         PERFORM  ZAIKO-SEC
         IF  KEP-FLG  =  SPACE
             MOVE     1       TO     DEN-F27D
         END-IF
         MOVE         1       TO     DEN-F262
*****#2018/04/02 NAV ST
         MOVE         SHO-F08 TO     WK-TANABAN
     ELSE
         MOVE         SPACE   TO     WK-TANABAN
*****#2018/04/02 NAV ED
     END-IF.
*2012/12/07 ↓　数量、単価、金額を登録時エリアにセット
     MOVE     DEN-F15         TO     DEN-F50.
     MOVE     DEN-F172        TO     DEN-F512.
     MOVE     DEN-F173        TO     DEN-F513.
     MOVE     DEN-F181        TO     DEN-F521.
     MOVE     DEN-F182        TO     DEN-F522.
*2012/12/07 ↑　数量、単価、金額を登録時エリアにセット
*****#2018/04/02 NAV ST 受注残ファイル更新
     PERFORM JYUZANF-UPD-SEC.
*****#2018/04/02 NAV ED
 AAA.
*伝票データ追加
     WRITE    DEN-REC.
 MAIN063.
*伝票番号を退避
     MOVE     DEN2-F02        TO     WK-DENBIKO.
     MOVE     DEN-F02         TO     WK-DENPYO.
     MOVE     DEN-F01         TO     WK-TORBIKO.
     PERFORM  DEN2-WT-SEC.
*伝票データ２リード
 DEN2-READ.
     PERFORM  DEN2-RD-SEC.
     IF    END-FLG  =  9
         PERFORM   BODY-WT-SEC
         PERFORM   BIKOU-ADD-SEC
*********2012/03/23 ST 伝票番号を最後に必ずカウントアップする
         PERFORM   DEN-GET-SEC
*********2012/03/23 END
         PERFORM   TOK-WT-SEC
     END-IF.
 MAIN-EXIT.
     EXIT.
****************************************************************
*               伝票データ２ＲＥＡＤ
****************************************************************
 DEN2-RD-SEC                 SECTION.
     MOVE     "DEN2-RD-SEC"       TO   S-NAME.
*
     READ     SHTDENWK  AT   END
              MOVE      9         TO   END-FLG
              GO        TO        DEN2-RD-EXIT.
 DEN2-RD-EXIT.
     EXIT.
****************************************************************
*              伝票データ２ ＲＥＷＲＩＴＥ
****************************************************************
 DEN2-WT-SEC                 SECTION.
     MOVE     "DEN2-WT-SEC"       TO   S-NAME.
*
     MOVE     WK-DENCNT           TO   DEN2-F02.
     MOVE     IX                  TO   DEN2-F03.
*****内部統制対応
     MOVE     WK-DEN-F59     TO        DEN2-F59.
     MOVE     WK-DEN-F60     TO        DEN2-F60.
     MOVE     WK-DEN-F61     TO        DEN2-F61.
     MOVE     WK-DEN-F62     TO        DEN2-F62.
     MOVE     WK-DEN-F63     TO        DEN2-F63.
     MOVE     WK-DEN-F64     TO        DEN2-F64.
     MOVE     WK-DEN-F65     TO        DEN2-F65.
     MOVE     WK-DEN-F66     TO        DEN2-F66.
     MOVE     ZERO           TO        DEN2-F67.
     MOVE     LINK-TANCD     TO        DEN2-F06.
*# 2020/04/27 NAV ST Ｄ３６５対応
     MOVE     WK-D365-PARA-OUT1   TO   DEN2-D99.
     MOVE     DEN-F32             TO   DEN2-F32.
*# 2020/04/27 NAV ED Ｄ３６５対応
*
     MOVE     DEN2-REC            TO   DME-REC.
*
     WRITE    DME-REC.
*
 DEN2-WT-EXIT.
     EXIT.
****************************************************************
*               取 引 先 マ ス タ Ｒ Ｅ Ａ Ｄ
****************************************************************
 TOK-RD-SEC                  SECTION.
     MOVE     "TOK-RD-SEC"        TO   S-NAME.
*
     MOVE     ZERO           TO   INV-SW
     MOVE     WK-TOKCD       TO   TOK-F01.
     READ     HTOKMS
       INVALID
              MOVE      1         TO   INV-SW
       NOT INVALID
              MOVE      TOK-F54   TO   WK-DENCNT
              MOVE      TOK-F03   TO   WK-TOKNM
              PERFORM   DEN-GET-SEC
              MOVE      ZERO      TO   IX
     END-READ.
 TOK-RD-EXIT.
     EXIT.
****************************************************************
*              取引先マスタ ＲＥＷＲＩＴＥ
****************************************************************
 TOK-WT-SEC                  SECTION.
     MOVE     "TOK-WT-SEC"        TO   S-NAME.
*
     MOVE     WK-DENCNT      TO   TOK-F54.
     REWRITE  TOK-REC.
 TOK-WT-EXIT.
     EXIT.
******************************************************************
*                  伝票番号取得
******************************************************************
 DEN-GET-SEC                 SECTION.
     MOVE     "DEN-GET-SEC"       TO   S-NAME.
*
     INITIALIZE              LINK-AREA.
     MOVE     TOK-F93        TO   LI-KBN.
     MOVE     TOK-F92        TO   LI-KETA.
     MOVE     TOK-F57        TO   LI-START.
     MOVE     TOK-F58        TO   LI-END.
     MOVE     WK-DENCNT      TO   LI-DENNO.
 DEN-010.
     CALL     "OSKTCDCK"     USING     LINK-AREA.
 DEN-020.
     IF       LO-ERR  =  0
              MOVE      LO-NEXT   TO   WK-DENCNT
              MOVE      LO-NEXT   TO   TOK-F54
     ELSE
              DISPLAY   NC"伝票_採番エラー"  UPON CONS
              DISPLAY   WK-DENCNT             UPON CONS
              DISPLAY   TOK-ST      UPON      CONS
**************DISPLAY   TOK-ST1     UPON      CONS
**************MOVE      255         TO        PROGRAM-STATUS
              ACCEPT    IN-DATA     FROM      CONS
              STOP  RUN
     END-IF.
*# 2020/04/27 NAV ST Ｄ３６５連携対応
     PERFORM  D365-DEN-SEC.
*# 2020/04/27 NAV ED Ｄ３６５連携対応
*
 DEN-GET-EXIT.
     EXIT.
****************************************************************
*             ヘッダ部出力処理                                 *
****************************************************************
 HEAD-WT-SEC                  SECTION.
     MOVE     "HEAD-WT-SEC"       TO   S-NAME.
*ページカウント
     ADD      1         TO        P-CNT.
*項目設定
***  日付
     MOVE     WK-Y                TO        HD1-01.
     MOVE     WK-M                TO        HD1-02.
     MOVE     WK-D                TO        HD1-03.
***  ページ_
     MOVE     P-CNT               TO        HD1-04.
***  ワークステーション名
     MOVE     LINK-WKSTNNM        TO        HD2-01.
*
*ヘッダ部出力
     WRITE    PRT-REC      FROM   HD1       AFTER  3.
     WRITE    PRT-REC      FROM   HD2       AFTER  1.
     WRITE    PRT-REC      FROM   HD3       AFTER  1.
     WRITE    PRT-REC      FROM   SEN       AFTER  1.
*
     ADD      6            TO        L-CNT.
 HEAD-WT-EXIT.
     EXIT.
****************************************************************
*             明細部出力処理　                                 *
****************************************************************
 BODY-WT-SEC                  SECTION.
     MOVE     "BODY-WT-SEC"       TO   S-NAME.
*
***  取引先コード
     MOVE     WK-TOKCD            TO   DT1-01.
***  取引先名
     MOVE     WK-TOKNM            TO   DT1-02.
***  開始伝票_
     MOVE     DENNO-S             TO   DT1-03.
***  終了伝票_
     MOVE     DENNO-L             TO   DT1-04.
*明細部出力
     WRITE    PRT-REC      FROM   DT1       AFTER  1.
*
     ADD      1            TO        L-CNT.
 BODY-WT-EXIT.
     EXIT.
****************************************************************
*      2.2     在庫引当                                        *
****************************************************************
 ZAIKO-SEC              SECTION.
*
     MOVE     "ZAIKO-SEC"         TO   S-NAME.
*商品在庫マスタ存在チェック
     MOVE    DEN-F08         TO   ZAI-F01.
     MOVE    DEN-F1411       TO   ZAI-F021.
     MOVE    DEN-F1412       TO   ZAI-F022.
     MOVE    SHO-F08         TO   ZAI-F03.
     READ    ZAMZAIF
             INVALID
             PERFORM   ZAIKO-UPDATE1-SEC
             NOT  INVALID
             PERFORM   ZAIKO-UPDATE2-SEC
     END-READ.
*
 ZAIKO-EXIT.
     EXIT.
****************************************************************
*      2.2     在庫引当                                        *
****************************************************************
 ZAIKO-UPDATE1-SEC      SECTION.
*
     MOVE     "ZAIKO-UPDATE1-SEC" TO   S-NAME.
*商品在庫Ｍが未存在の為、在庫マスタ作成
      MOVE      "1"           TO   KEP-FLG.
*商品在庫マスタ初期化
      MOVE      SPACE         TO   ZAI-REC.
      INITIALIZE                   ZAI-REC.
*商品在庫マスタ項目セット
      MOVE      DEN-F08       TO   ZAI-F01.
      MOVE      DEN-F1411     TO   ZAI-F021.
      MOVE      DEN-F1412     TO   ZAI-F022.
      MOVE      SHO-F08       TO   ZAI-F03.
*未出庫数＝未出庫数＋数量
      COMPUTE   ZAI-F27       =    ZAI-F27  +  DEN-F15.
*商品名称マスタ読込み
      PERFORM   HMEIMS-READ-SEC.
*商品名称マスタ存在チェック
      IF  HMEIMS-INV-FLG  =  SPACE
          MOVE  WK-TOKCD      TO   ZAI-F29
          MOVE  MEI-F031      TO   ZAI-F30
          MOVE  WK-DATE8      TO   ZAI-F98
          MOVE  WK-DATE8      TO   ZAI-F99
          WRITE ZAI-REC
      END-IF.
*
 ZAIKO-UPDATE1-EXIT.
      EXIT.
****************************************************************
*      2.2     在庫引当                                        *
****************************************************************
 ZAIKO-UPDATE2-SEC      SECTION.
*
     MOVE     "ZAIKO-UPDATE2-SEC" TO   S-NAME.
*引当後在庫数チェック
*    現在庫数－引当済数＝引当可能在庫数
     COMPUTE   WRK-ZAI   =   ZAI-F04  -  ZAI-F28.
*    引当可能在庫数－発注数量＝引当後在庫数
     COMPUTE   WRK-HIK   =   WRK-ZAI  -  DEN-F15.
     IF  WRK-HIK  <  0
         MOVE      "1"      TO   KEP-FLG
*        未出庫数に数量加算
         COMPUTE  ZAI-F27   =    ZAI-F27  +  DEN-F15
         MOVE     WK-DATE8  TO   ZAI-F99
*        商品在庫マスタ更新
         REWRITE  ZAI-REC
     ELSE
         MOVE     SPACE          TO   KEP-FLG
*        引当済数に数量加算
         COMPUTE  ZAI-F28   =    ZAI-F28  +  DEN-F15
*        未出庫数に数量加算
         COMPUTE  ZAI-F27   =    ZAI-F27  +  DEN-F15
         MOVE     WK-DATE8  TO   ZAI-F99
*        商品在庫マスタ更新
         REWRITE  ZAI-REC
     END-IF.
*
 ZAIKO-UPDATE2-EXIT.
     EXIT.
****************************************************************
*                商品変換テーブル読込み                        *
****************************************************************
 HSHOTBL-READ-SEC          SECTION.
*
     MOVE      "HSHOTBL-READ-SEC" TO    S-NAME.
*
     MOVE      DEN-F01     TO     SHO-F01.
     MOVE      DEN-F25     TO     SHO-F02.
     READ      HSHOTBL
               INVALID
               MOVE      "INV"    TO    HSHOTBL-INV-FLG
*2009.09.14
               MOVE      ZERO     TO    SHO-F09
               NOT  INVALID
               MOVE      SPACE    TO    HSHOTBL-INV-FLG
     END-READ.
*
 HSHOTBL-READ-EXIT.
     EXIT.
****************************************************************
*                商品名称マスタ読込み                          *
****************************************************************
 HMEIMS-READ-SEC           SECTION.
*
     MOVE      "HMEIMS-READ-SEC"  TO    S-NAME.
*
     MOVE      SHO-F031    TO     MEI-F011.
     MOVE      SHO-F032    TO     MEI-F012.
     READ      HMEIMS
               INVALID
               MOVE      "INV"    TO    HMEIMS-INV-FLG
               NOT  INVALID
               MOVE      SPACE    TO    HMEIMS-INV-FLG
     END-READ.
*
 HMEIMS-READ-EXIT.
     EXIT.
****************************************************************
*    備考明細チェック
****************************************************************
 BIKOU-ADD-SEC             SECTION.
*
     MOVE       WK-TORBIKO     TO        BIK-F01.
     MOVE       WK-DENBIKO     TO        BIK-F02.
     MOVE       80             TO        BIK-F03.
     READ       SHTBIKF
                INVALID
                MOVE    "INV"  TO        BIKO-INV-FLG
                NOT INVALID
                MOVE    SPACE  TO        BIKO-INV-FLG
     END-READ.
*
     IF  BIKO-INV-FLG  =  SPACE
                MOVE  SPACE    TO        DEN-REC DME-REC
                INITIALIZE               DEN-REC DME-REC
                MOVE  BIK-REC   TO       DEN-REC DME-REC
                MOVE  WK-DENPYO TO       DEN-F02 DME-F02
                MOVE  WK-DENPYO TO       DEN-F23 DME-F23
                MOVE  80        TO       DEN-F03 DME-F03
                MOVE  WK-TENCD  TO       DEN-F07 DME-F07
                MOVE  WK-SYUKA  TO       DEN-F112 DME-F112
****************内部統制対応
                MOVE  WK-DEN-F59 TO      DEN-F59 DME-F59
                MOVE  WK-DEN-F60 TO      DEN-F60 DME-F60
                MOVE  WK-DEN-F61 TO      DEN-F61 DME-F61
                MOVE  WK-DEN-F62 TO      DEN-F62 DME-F62
                MOVE  WK-DEN-F63 TO      DEN-F63 DME-F63
                MOVE  WK-DEN-F64 TO      DEN-F64 DME-F64
                MOVE  WK-DEN-F65 TO      DEN-F65 DME-F65
                MOVE  WK-DEN-F66 TO      DEN-F66 DME-F66
                MOVE  ZERO       TO      DEN-F67 DME-F67
                MOVE  LINK-TANCD TO      DEN-F06 DME-F06
                WRITE DME-REC
                WRITE DEN-REC
     END-IF.
*
 BIKOU-ADD-EXIT.
     EXIT.
****************************************************************
*    終了
****************************************************************
 END-SEC                   SECTION.
*
*     DISPLAY  "＊＊量販店　データ変換＊＊＊" UPON CONS.
*     DISPLAY  "＊＊開始伝票_　＝　" DENNO-S UPON CONS.
*     DISPLAY  "＊＊終了伝票_　＝　" DENNO-L UPON CONS.
*
     CLOSE SHTDENWK SHTDENF PRTFILE HTOKMS HSHOTBL HMEIMS ZAMZAIF
           SHTBIKF  DENMEISF  HTANMS.
*# 2020/04/27 NAV ST Ｄ３６５連携対応
     CLOSE  SUBTBLF.
*# 2020/04/27 NAV ED Ｄ３６５連携対応
 END-EXIT.
     EXIT.
****************************************************************
*    担当者マスタ索引
****************************************************************
 TAN-READ-SEC              SECTION.
*
     INITIALIZE                          WK-TOUSEI.
     MOVE       SPACE          TO        TAN-REC.
     INITIALIZE                          TAN-REC.
     MOVE       LINK-BUMON     TO        TAN-F01.
     MOVE       LINK-TANCD     TO        TAN-F02.
     READ       HTANMS
                INVALID
                MOVE    "INV"  TO        TAN-INV-FLG
                NOT INVALID
                MOVE    SPACE  TO        TAN-INV-FLG
     END-READ.
*
     IF  TAN-INV-FLG   =  SPACE
         IF   TAN-F06  = "1" OR "2" OR "3"
**************登録更新承認担当者・日付・代表権限・権限１
              MOVE    LINK-TANCD   TO    WK-DEN-F59
              MOVE    LINK-TANCD   TO    WK-DEN-F60
              MOVE    LINK-TANCD   TO    WK-DEN-F61
              MOVE    WK-DATE8     TO    WK-DEN-F62
              MOVE    WK-DATE8     TO    WK-DEN-F63
              MOVE    WK-DATE8     TO    WK-DEN-F64
              MOVE    TAN-F05      TO    WK-DEN-F65
              MOVE    TAN-F06      TO    WK-DEN-F66
         ELSE
**************登録更新承認担当者・日付・代表権限・権限１
              MOVE    LINK-TANCD   TO    WK-DEN-F59
              MOVE    LINK-TANCD   TO    WK-DEN-F60
              MOVE    SPACE        TO    WK-DEN-F61
              MOVE    WK-DATE8     TO    WK-DEN-F62
              MOVE    WK-DATE8     TO    WK-DEN-F63
              MOVE    ZERO         TO    WK-DEN-F64
              MOVE    TAN-F05      TO    WK-DEN-F65
              MOVE    TAN-F06      TO    WK-DEN-F66
         END-IF
     ELSE
         DISPLAY NC"担当者Ｍ未登録" DEN2-F06 UPON CONS
*********登録更新承認担当者・日付・代表権限・権限１
         MOVE    DEN2-F06          TO    WK-DEN-F59
         MOVE    DEN2-F06          TO    WK-DEN-F60
         MOVE    SPACE             TO    WK-DEN-F61
         MOVE    WK-DATE8          TO    WK-DEN-F62
         MOVE    WK-DATE8          TO    WK-DEN-F63
         MOVE    ZERO              TO    WK-DEN-F64
         MOVE    TAN-F05           TO    WK-DEN-F65
         MOVE    TAN-F06           TO    WK-DEN-F66
     END-IF.
*
 TAN-READ-EXIT.
     EXIT.
*#2018/04/02 NAV ST
*--------------------------------------------------------------*
*    受注残ファイル更新処理
*--------------------------------------------------------------*
 JYUZANF-UPD-SEC        SECTION.
*
     MOVE      "1"                     TO   PARA-KOSINKBN.
     MOVE      "2"                     TO   PARA-DTKBN.
     MOVE      DEN-F01                 TO   PARA-TOKCD.
     MOVE      DEN-F02                 TO   PARA-DENNO.
     MOVE      DEN-F03                 TO   PARA-GYO.
     MOVE      DEN-F04                 TO   PARA-SOUSAI.
     MOVE      DEN-F051                TO   PARA-DENKU.
     MOVE      DEN-F07                 TO   PARA-TENCD.
     MOVE      DEN-F08                 TO   PARA-SOKCD.
     MOVE      DEN-F111                TO   PARA-HATYU.
     MOVE      DEN-F112                TO   PARA-NOUHIN.
     MOVE      DEN-F113                TO   PARA-SYUKA.
     MOVE      DEN-F1411               TO   PARA-SAKATACD.
     MOVE      DEN-F1412(1:5)          TO   PARA-HINTAN1.
     MOVE      DEN-F1412(6:2)          TO   PARA-HINTAN2.
     MOVE      DEN-F1412(8:1)          TO   PARA-HINTAN3.
     MOVE      WK-TANABAN              TO   PARA-TANABAN.
*****手書伝票入力の場合は、受注数、出荷数とも画面の数量を更新
     MOVE      DEN-F15                 TO   PARA-JYUTYUSU.
     MOVE      DEN-F15                 TO   PARA-SYUKASU.
     MOVE      DEN-F172                TO   PARA-GENKA.
     MOVE      DEN-F173                TO   PARA-BAIKA.
     MOVE      DEN-F181                TO   PARA-GENKAKIN.
     MOVE      DEN-F182                TO   PARA-BAIKAKIN.
     MOVE      DEN-F25                 TO   PARA-AITECD.
     MOVE      DEN-F27D                TO   PARA-HIKIATE.
     MOVE      LINK-BUMON              TO   PARA-UPBUMON
                                            PARA-BUMON.
     MOVE      LINK-TANCD              TO   PARA-UPTANCD
                                            PARA-TANCD.
     MOVE      DEN-F46                 TO   PARA-JDATE.
     MOVE      DEN-F47                 TO   PARA-JTIME.
*サブルーチンコール
     CALL    "SJZ0110B" USING PARA-AREA PARA-BUMON PARA-TANCD.
*
 JYUZANF-UPD-EXIT.
     EXIT.
*#2018/04/02 NAV ED
****************************************************************
*          ＳＵＢ商品変換テーブル読込み                        *
****************************************************************
 SUBTBLF-READ-SEC          SECTION.
*
     MOVE      "SUBTBLF-READ-SEC" TO    S-NAME.
*
     MOVE      DEN-F01     TO     SUB-F01.
     MOVE      DEN-F25     TO     SUB-F02.
     READ      SUBTBLF
               INVALID
               MOVE      "INV"    TO    SUBTBLF-INV-FLG
               NOT  INVALID
               MOVE      SPACE    TO    SUBTBLF-INV-FLG
     END-READ.
*
 SUBTBLF-READ-EXIT.
     EXIT.
****************************************************************
*     Ｄ３６５伝票番号採番　　　　　　　                       *
****************************************************************
 D365-DEN-SEC              SECTION.
*
     MOVE      "D365-DEN-SEC"     TO    S-NAME.
*
     INITIALIZE                   WK-D365-DEN-PARA.
*****伝票区分でＤ３６５伝票番号の先頭１桁を変更
     IF   DEN2-F051  =  "40"
          MOVE "T"         TO     WK-D365-PARA-IN1
     ELSE
          IF   DEN2-F051  =  "41"
               MOVE "H"    TO     WK-D365-PARA-IN1
          ELSE
               MOVE "T"    TO     WK-D365-PARA-IN1
          END-IF
     END-IF.
     MOVE      DEN2-F01    TO     WK-D365-PARA-IN2.
     CALL      "SKYD3DEN"  USING  WK-D365-PARA-IN1
                                  WK-D365-PARA-IN2
                                  WK-D365-PARA-OUT1
                                  WK-D365-PARA-OUT2.
     IF   WK-D365-PARA-OUT2  =  "1"
          DISPLAY "## " NC"異常" "D365NO" NC"採番" " ERR]] ##"
          MOVE   4000      TO      PROGRAM-STATUS
          STOP  RUN
     END-IF.
*****DISPLAY "WK-D365-PARA-OUT1 = " WK-D365-PARA-OUT1 UPON CONS.
*
 D365-DEN-EXIT.
     EXIT.
