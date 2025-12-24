****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　営利種子ストックＮＯ管理　　　　　*
*    業務名　　　　　　　：　営利種子ストックＮＯ管理　        *
*    モジュール名　　　　：　ストックＮＯ実績データ出力　　    *
*    作成日／更新日　　　：　20/07/14                          *
*    作成者／更新者　　　：　ＮＡＶ高橋                        *
*    処理概要　　　　　　：　パラメタを受取、ＣＳＶ出力。　　　*
*    更新日／更新者　　　：　                                  *
*    更新概要　　　　　　：　　　　　　　　　　　　            *
*                                                              *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SST0050V.
 AUTHOR.               NAV.
 DATE-WRITTEN.         20/07/14.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     YA           IS   CHR-2
     YB-21        IS   CHR-21
     YB           IS   CHR-15
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*ストックＮＯ管理ファイル
     SELECT  STNOKRF   ASSIGN    TO        STNOKRL2
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      SEQUENTIAL
                       RECORD    KEY       STN-F03
                                           STN-F05
                                           STN-F04
                                           STN-F06
                                           STN-F07
                                           STN-F08
                       FILE      STATUS    STN-ST.
*
*取引先マスタ
     SELECT  HTOKMS    ASSIGN    TO        TOKMS2
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       TOK-F01
                       FILE      STATUS    TOK-ST.
*
*店舗マスタ
     SELECT  HTENMS    ASSIGN    TO        TENMS1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       TEN-F52  TEN-F011
                       FILE      STATUS    TEN-ST.
*
*倉庫マスタ
     SELECT  ZSOKMS    ASSIGN    TO        ZSOKMS1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       SOK-F01
                       FILE      STATUS    SOK-ST.
*
*商品名称マスタ　　
     SELECT  HMEIMS    ASSIGN    TO        MEIMS1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       MEI-F011  MEI-F0121
                                           MEI-F0122 MEI-F0123
                       FILE      STATUS    MEI-ST.
*ストックＮＯ実績データＣＳＶ
     SELECT  STNOJCSV  ASSIGN    TO        STNOJCSV
                       FILE      STATUS    CSV-ST.
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = ストックＮＯ管理ファイル（Ｌ２）                   *
****************************************************************
 FD  STNOKRF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      STNOKRF   OF   XFDLIB
                       JOINING   STN       AS   PREFIX.
****************************************************************
*    FILE = 取引先マスタ                                       *
****************************************************************
 FD  HTOKMS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      HTOKMS    OF   XFDLIB
                       JOINING   TOK       AS   PREFIX.
****************************************************************
*    FILE = 店舗マスタ　                                       *
****************************************************************
 FD  HTENMS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      HTENMS    OF   XFDLIB
                       JOINING   TEN       AS   PREFIX.
****************************************************************
*    FILE = 倉庫マスタ　　                                     *
****************************************************************
 FD  ZSOKMS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      ZSOKMS    OF   XFDLIB
                       JOINING   SOK       AS   PREFIX.
****************************************************************
*    FILE = 商品名称マスタ　                                   *
****************************************************************
 FD  HMEIMS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      HMEIMS    OF   XFDLIB
                       JOINING   MEI       AS   PREFIX.
****************************************************************
*    FILE = ストックＮＯ実績データＣＳＶ                       *
****************************************************************
 FD  STNOJCSV          BLOCK CONTAINS 1    RECORDS.
 01  CSV-REC.
     03  FILLER        PIC  X(1500).
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  STN-ST                   PIC  X(02).
     03  TOK-ST                   PIC  X(02).
     03  TEN-ST                   PIC  X(02).
     03  SOK-ST                   PIC  X(02).
     03  MEI-ST                   PIC  X(02).
     03  CSV-ST                   PIC  X(02).
*フラグ領域
 01  FLG-AREA.
     03  TAISYO-FLG               PIC  X(01)  VALUE  SPACE.
     03  ERR-FLG                  PIC  9(02)  VALUE  ZERO.
     03  END-FLG                  PIC  X(03)  VALUE  SPACE.
     03  HTOKMS-INV-FLG           PIC  X(03)  VALUE  SPACE.
     03  HTENMS-INV-FLG           PIC  X(03)  VALUE  SPACE.
     03  ZSOKMS-INV-FLG           PIC  X(03)  VALUE  SPACE.
     03  HMEIMS-INV-FLG           PIC  X(03)  VALUE  SPACE.
*カウンタ領域
 01  COUNTER.
     03  READ-CNT                 PIC  9(08)  VALUE  ZERO.
     03  CSV-CNT                  PIC  9(08)  VALUE  ZERO.
     03  TAISYO-CNT               PIC  9(06)  VALUE  ZERO.
     03  WK-GK-SYUKASU            PIC  9(06)  VALUE  ZERO.
*ワーク領域
 01  WRK-AREA.
***  プログラムスイッチ（画面遷移制御）
     03  PSW                      PIC  X(01)  VALUE  SPACE.
***  インデックス
     03  IXA                      PIC  9(02)  VALUE  ZERO.
***  エラーセクション名
 01  SEC-NAME.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-SEC    => ".
     03  S-NAME                   PIC  X(20).
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
*受信時間チェック
 01  WK-JIKAN.
     03  WK-HH                    PIC   9(02)  VALUE  ZERO.
     03  WK-MM                    PIC   9(02)  VALUE  ZERO.
*
*発注日、納品日編集
 01  WK-HENKAN-DATE               PIC   9(08)  VALUE  ZERO.
 01  WK-OUT-DATE.
     03  WK-OUT-YYYY              PIC   9(04)  VALUE  ZERO.
     03  WK-OUT-MM                PIC   9(02)  VALUE  ZERO.
     03  WK-OUT-DD                PIC   9(02)  VALUE  ZERO.
*
*日付論理チェック
 01  WK-CHKDATE.
     03  WK-CHKDATE-YYYY          PIC   9(04)  VALUE  ZERO.
     03  WK-CHKDATE-MM            PIC   9(02)  VALUE  ZERO.
     03  WK-CHKDATE-DD            PIC   9(02)  VALUE  ZERO.
*
 01  FILE-ERR.
     03  STN-ERR           PIC N(20) VALUE
                        NC"ストックＮＯ管理Ｆエラー".
     03  TOK-ERR           PIC N(20) VALUE
                        NC"取引先マスタエラー".
     03  TEN-ERR           PIC N(20) VALUE
                        NC"店舗マスタエラー".
     03  SOK-ERR           PIC N(20) VALUE
                        NC"倉庫マスタエラー".
     03  MEI-ERR           PIC N(20) VALUE
                        NC"商品名称マスタエラー".
     03  CSV-ERR           PIC N(20) VALUE
         NC"ストックＮＯ実績データＣＳＶエラー".
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*
*見出しエリア
 01  WK-HEAD1.
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(13)  VALUE
                       NC"＜ストックＮＯ実績データ＞".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"実行日：".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  H01           PIC X(10).
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(05)  VALUE  NC"実行時刻：".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  H02           PIC X(08).
 01  WK-HEAD2.
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(05)  VALUE  NC"バッチ日付".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(05)  VALUE  NC"バッチ時刻".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(06)  VALUE  NC"バッチ取引先".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"取引先名".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"倉庫ＣＤ".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(03)  VALUE  NC"倉庫名".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(03)  VALUE  NC"納品日".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"店舗ＣＤ".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(03)  VALUE  NC"店舗名".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"伝票番号".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(03)  VALUE  NC"行番号".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(06)  VALUE  NC"相手商品ＣＤ".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(07)  VALUE  NC"サカタ商品ＣＤ".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(07)  VALUE  NC"サカタ品単ＣＤ".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"商品名１".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"商品名２".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(03)  VALUE  NC"発注数".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(03)  VALUE  NC"出荷数".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"原価単価".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"売価単価".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"原価金額".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(04)  VALUE  NC"売価金額".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(07)  VALUE  NC"ストックＮＯ１".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(05)  VALUE  NC"数量内訳１".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(07)  VALUE  NC"ストックＮＯ２".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(05)  VALUE  NC"数量内訳２".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(07)  VALUE  NC"ストックＮＯ３".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(05)  VALUE  NC"数量内訳３".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(07)  VALUE  NC"ストックＮＯ４".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(05)  VALUE  NC"数量内訳４".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(07)  VALUE  NC"ストックＮＯ５".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".
     03  FILLER        PIC X(01)  VALUE  X"28".
     03  FILLER        PIC N(05)  VALUE  NC"数量内訳５".
     03  FILLER        PIC X(01)  VALUE  X"29".
     03  FILLER        PIC X(01)  VALUE  ",".

*明細エリア
 01  WK-MEISAI1.
     03  M01           PIC X(10).
     03  MK01          PIC X(01).
     03  M02           PIC X(08).
     03  MK02          PIC X(01).
     03  M03           PIC 9(08).
     03  MK03          PIC X(01).
     03  M04S          PIC X(01).
     03  M04           PIC N(15).
     03  M04E          PIC X(01).
     03  MK04          PIC X(01).
     03  M05           PIC X(02).
     03  MK05          PIC X(01).
     03  M06S          PIC X(01).
     03  M06           PIC N(15).
     03  M06E          PIC X(01).
     03  MK06          PIC X(01).
     03  M07           PIC X(10).
     03  MK07          PIC X(01).
     03  M08           PIC 9(05).
     03  MK08          PIC X(01).
     03  M09S          PIC X(01).
     03  M09           PIC N(30).
     03  M09E          PIC X(01).
     03  MK09          PIC X(01).
     03  M091          PIC 9(08).
     03  MK091         PIC X(01).
     03  M092          PIC 9(02).
     03  MK092         PIC X(01).
     03  M10           PIC X(13).
     03  MK10          PIC X(01).
     03  M11           PIC X(08).
     03  MK11          PIC X(01).
     03  M12           PIC X(08).
     03  MK12          PIC X(01).
     03  M13S          PIC X(01).
     03  M13           PIC N(15).
     03  M13E          PIC X(01).
     03  MK13          PIC X(01).
     03  M14S          PIC X(01).
     03  M14           PIC N(15).
     03  M14E          PIC X(01).
     03  MK14          PIC X(01).
     03  M15           PIC 9(11).
     03  MK15          PIC X(01).
     03  M16           PIC 9(11).
     03  MK16          PIC X(01).
     03  M17           PIC 9(11).
     03  MK17          PIC X(01).
     03  M18           PIC 9(11).
     03  MK18          PIC X(01).
     03  M19           PIC 9(11).
     03  MK19          PIC X(01).
     03  M20           PIC 9(11).
     03  MK20          PIC X(01).
     03  M21           PIC X(06).
     03  MK21          PIC X(01).
     03  M22           PIC 9(11).
     03  MK22          PIC X(01).
     03  M23           PIC X(06).
     03  MK23          PIC X(01).
     03  M24           PIC 9(11).
     03  MK24          PIC X(01).
     03  M25           PIC X(06).
     03  MK25          PIC X(01).
     03  M26           PIC 9(11).
     03  MK26          PIC X(01).
     03  M27           PIC X(06).
     03  MK27          PIC X(01).
     03  M28           PIC 9(11).
     03  MK28          PIC X(01).
     03  M29           PIC X(06).
     03  MK29          PIC X(01).
     03  M30           PIC 9(11).
     03  MK30          PIC X(01).
 LINKAGE                   SECTION.
 01  PARA-TOKCD                  PIC  9(08).
 01  PARA-NOUDTS                 PIC  9(08).
 01  PARA-NOUDTE                 PIC  9(08).
 01  PARA-SOKCDS                 PIC  X(02).
 01  PARA-SOKCDE                 PIC  X(02).
 01  PARA-JANCDS                 PIC  X(13).
 01  PARA-JANCDE                 PIC  X(13).
 01  PARA-STNOS                  PIC  X(06).
 01  PARA-STNOE                  PIC  X(06).
 01  PARA-KBN1                   PIC  X(01).
 01  PARA-KBN2                   PIC  X(01).
**************************************************************
 PROCEDURE             DIVISION  USING  PARA-TOKCD
                                        PARA-NOUDTS
                                        PARA-NOUDTE
                                        PARA-SOKCDS
                                        PARA-SOKCDE
                                        PARA-JANCDS
                                        PARA-JANCDE
                                        PARA-STNOS
                                        PARA-STNOE
                                        PARA-KBN1
                                        PARA-KBN2.
**************************************************************
 DECLARATIVES.
 STN-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE STNOKRF.
     DISPLAY     STN-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     STN-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 TOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HTOKMS.
     DISPLAY     TOK-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     TOK-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 TEN-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HTENMS.
     DISPLAY     TEN-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     TEN-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 SOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE ZSOKMS.
     DISPLAY     SOK-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     SOK-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 MEI-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HMEIMS.
     DISPLAY     MEI-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     MEI-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 CSV-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE STNOJCSV.
     DISPLAY     CSV-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     CSV-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 END  DECLARATIVES.
****************************************************************
*             MAIN        MODULE                     0.0       *
****************************************************************
 PROCESS-START         SECTION.
     MOVE     "PROCESS START"     TO   S-NAME.
***
     PERFORM   INIT-SEC.
     PERFORM   MAIN-SEC          UNTIL   END-FLG   =   "END".
     PERFORM   END-SEC.
***
     STOP    RUN.
 PROCESS-EXIT.
     EXIT.
****************************************************************
*             初期処理                               1.0
****************************************************************
 INIT-SEC              SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
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
*システム時間取得
     ACCEPT    WK-TIME          FROM   TIME.
*画面表示時刻編集
     MOVE      WK-TIME(1:2)       TO   HEN-TIME-HH.
     MOVE      WK-TIME(3:2)       TO   HEN-TIME-MM.
     MOVE      WK-TIME(5:2)       TO   HEN-TIME-SS.
*ファイルのＯＰＥＮ
     OPEN     INPUT  STNOKRF  HTOKMS  HTENMS  ZSOKMS  HMEIMS.
     OPEN     OUTPUT STNOJCSV.
*ワークの初期化
     INITIALIZE         FLG-AREA.
*カウンタの初期化
     INITIALIZE         COUNTER.
*ストックＮＯ管理ファイル存在チェック
     MOVE     SPACE          TO   STN-REC.
     INITIALIZE                   STN-REC.
     MOVE     PARA-TOKCD     TO   STN-F03.
     MOVE     PARA-NOUDTS    TO   STN-F05.
     MOVE     PARA-SOKCDS    TO   STN-F04.
     START    STNOKRF   KEY  IS   >=   STN-F03
                                       STN-F05
                                       STN-F04
                                       STN-F06
                                       STN-F07
                                       STN-F08
        INVALID
              MOVE  "END"    TO   END-FLG
              DISPLAY NC"＃対象データ無１＃" UPON CONS
              MOVE  4010     TO   PROGRAM-STATUS
              GO             TO   INIT-EXIT
     END-START.
*ストックＮＯ管理ファイル読込
     PERFORM  STNOKRF-READ-SEC.
     IF   END-FLG  =  "END"
          DISPLAY NC"＃対象データ無２＃" UPON CONS
          MOVE  4010     TO   PROGRAM-STATUS
          GO             TO   INIT-EXIT
     ELSE
          MOVE  SPACE    TO   CSV-REC
          MOVE  HEN-DATE TO   H01
          MOVE  HEN-TIME TO   H02
          MOVE  WK-HEAD1 TO   CSV-REC
          WRITE CSV-REC
          MOVE  SPACE    TO   CSV-REC
          MOVE  WK-HEAD2 TO   CSV-REC
          WRITE CSV-REC
          ADD    2       TO   CSV-CNT
     END-IF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*             メイン処理                             2.0
****************************************************************
 MAIN-SEC              SECTION.
     MOVE     "MAIN-SEC"     TO   S-NAME.
*
     MOVE      SPACE         TO   CSV-REC  WK-MEISAI1.
*    ＣＳＶ（カンマ）セット
     MOVE      ","           TO   MK01 MK02 MK03 MK04 MK05 MK06
         MK07 MK08 MK09 MK10 MK11 MK12 MK13 MK14 MK15 MK16 MK17
         MK18 MK19 MK20 MK21 MK22 MK23 MK24 MK25 MK26 MK27 MK28
         MK29 MK30 MK091 MK092.
*    制御バイトセット
     MOVE X"28"              TO   M04S M06S M09S M13S M14S.
     MOVE X"29"              TO   M04E M06E M09E M13E M14E.
*    バッチ日付
     MOVE  STN-F01(1:4)      TO   HEN-DATE-YYYY.
     MOVE  STN-F01(5:2)      TO   HEN-DATE-MM.
     MOVE  STN-F01(7:2)      TO   HEN-DATE-DD.
     MOVE  HEN-DATE          TO   M01.
*    バッチ時刻
     MOVE  STN-F02(1:2)      TO   HEN-TIME-HH.
     MOVE  STN-F02(3:2)      TO   HEN-TIME-MM.
     MOVE  "00"              TO   HEN-TIME-SS.
     MOVE  HEN-TIME          TO   M02.
*    バッチ取引先
     MOVE  STN-F03           TO   M03.
*    取引先名
     PERFORM  HTOKMS-READ-SEC.
     IF  HTOKMS-INV-FLG  =  "INV"
         MOVE ALL NC"＊"     TO   M04
     ELSE
         MOVE TOK-F02        TO   M04
     END-IF.
*    倉庫ＣＤ
     MOVE  STN-F04           TO   M05.
*    倉庫名　　
     PERFORM  ZSOKMS-READ-SEC.
     IF  ZSOKMS-INV-FLG  =  "INV"
         MOVE ALL NC"＊"     TO   M06
     ELSE
         MOVE SOK-F02        TO   M06
     END-IF.
*    バッチ日付
     MOVE  STN-F05(1:4)      TO   HEN-DATE-YYYY.
     MOVE  STN-F05(5:2)      TO   HEN-DATE-MM.
     MOVE  STN-F05(7:2)      TO   HEN-DATE-DD.
     MOVE  HEN-DATE          TO   M07.
*    店舗ＣＤ
     MOVE  STN-F06           TO   M08.
*    店舗名　　
     PERFORM  HTENMS-READ-SEC.
     IF  HTENMS-INV-FLG  =  "INV"
         MOVE ALL NC"＊"     TO   M09
     ELSE
         MOVE TEN-F02        TO   M09
     END-IF.
*    伝票番号
     MOVE  STN-F07           TO   M091.
*    行番号　　　　　
     MOVE  STN-F08           TO   M092.
*    相手商品ＣＤ
     MOVE  STN-F19           TO   M10.
*    サカタ商品ＣＤ
     MOVE  STN-F11           TO   M11.
*    サカタ品単ＣＤ
     MOVE  STN-F12           TO   M12.
*    商品名　　
     PERFORM  HMEIMS-READ-SEC.
     IF  HMEIMS-INV-FLG  =  "INV"
         MOVE ALL NC"＊"     TO   M13  M14
     ELSE
         MOVE MEI-F021       TO   M13
         MOVE MEI-F022       TO   M14
     END-IF.
*    発注数
     MOVE  STN-F13           TO   M15.
*    出荷数
     MOVE  STN-F14           TO   M16.
*    原価単価
     MOVE  STN-F15           TO   M17.
*    売価単価
     MOVE  STN-F16           TO   M18.
*    原価金額
     MOVE  STN-F17           TO   M19.
*    売価金額
     MOVE  STN-F18           TO   M20.
*    ストックＮＯ１
     MOVE  STN-F30           TO   M21.
*    数量内訳１
     MOVE  STN-F31           TO   M22.
*    ストックＮＯ２
     MOVE  STN-F32           TO   M23.
*    数量内訳２
     MOVE  STN-F33           TO   M24.
*    ストックＮＯ３
     MOVE  STN-F34           TO   M25.
*    数量内訳３
     MOVE  STN-F35           TO   M26.
*    ストックＮＯ４
     MOVE  STN-F36           TO   M27.
*    数量内訳４
     MOVE  STN-F37           TO   M28.
*    ストックＮＯ５
     MOVE  STN-F38           TO   M29.
*    数量内訳５
     MOVE  STN-F39           TO   M30.
*　　明細出力
     MOVE  WK-MEISAI1        TO   CSV-REC.
     WRITE CSV-REC.
     ADD   1                 TO   CSV-CNT.
*ストックＮＯ管理ファイル読込
     PERFORM  STNOKRF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*             終了処理                               6.0       *
****************************************************************
 END-SEC               SECTION.
*ファイル ＣＬＯＳＥ
     CLOSE             STNOKRF  HTOKMS  HTENMS  ZSOKMS  HMEIMS
                       STNOJCSV.
**
     DISPLAY "# READ-CNT = " READ-CNT    UPON  CONS.
     DISPLAY "# CSV-CNT    " CSV-CNT     UPON  CONS.
**
 END-EXIT.
     EXIT.
****************************************************************
*    ストックＮＯ管理ファイル読込　　　　　　　　　　　　      *
****************************************************************
 STNOKRF-READ-SEC     SECTION.
     MOVE "STNOKRF-READ-SEC"     TO   S-NAME.
     READ    STNOKRF
             AT  END
             MOVE  "END"     TO   END-FLG
             GO              TO   STNOKRF-READ-EXIT
             NOT  AT  END
             ADD   1         TO   READ-CNT
     END-READ.
*件数表示
     IF  READ-CNT(6:3) =  "000" OR  "500"
         DISPLAY "READ-CNT = " READ-CNT " #" UPON CONS
     END-IF.
*取引先ＣＤチェック
     IF  PARA-TOKCD   NOT =  STN-F03
         MOVE  "END"         TO   END-FLG
         GO                  TO   STNOKRF-READ-EXIT
     END-IF.
*納品日範囲チェック
     IF  PARA-NOUDTS  <=  STN-F05
     AND PARA-NOUDTE  >=  STN-F05
         CONTINUE
     ELSE
         MOVE  "END"         TO   END-FLG
         GO                  TO   STNOKRF-READ-EXIT
     END-IF.
*倉庫ＣＤ範囲チェック        CONTINUE
     IF  PARA-SOKCDS  <=  STN-F04
     AND PARA-SOKCDE  >=  STN-F04
         CONTINUE
     ELSE
         GO                  TO   STNOKRF-READ-SEC
     END-IF.
*相手商品ＣＤ範囲チェック
     IF  PARA-JANCDS  <=  STN-F19
     AND PARA-JANCDE  >=  STN-F19
         CONTINUE
     ELSE
         GO                  TO   STNOKRF-READ-SEC
     END-IF.
*ストックＮＯ範囲チェック
     IF  PARA-STNOS  <=  STN-F30
     AND PARA-STNOE  >=  STN-F30
         GO                  TO   STNOKRF-READ-010
     END-IF.
     IF  PARA-STNOS  <=  STN-F32
     AND PARA-STNOE  >=  STN-F32
         GO                  TO   STNOKRF-READ-010
     END-IF.
     IF  PARA-STNOS  <=  STN-F34
     AND PARA-STNOE  >=  STN-F34
         GO                  TO   STNOKRF-READ-010
     END-IF.
     IF  PARA-STNOS  <=  STN-F36
     AND PARA-STNOE  >=  STN-F36
         GO                  TO   STNOKRF-READ-010
     END-IF.
     IF  PARA-STNOS  <=  STN-F38
     AND PARA-STNOE  >=  STN-F38
         CONTINUE
     ELSE
         GO                  TO   STNOKRF-READ-SEC
     END-IF.
 STNOKRF-READ-010.
*ストックＮＯ入力チェック
     IF  PARA-KBN1  =  "1"
         IF  STN-F30  =  SPACE
         AND STN-F32  =  SPACE
         AND STN-F34  =  SPACE
         AND STN-F36  =  SPACE
         AND STN-F38  =  SPACE
             CONTINUE
         ELSE
             GO              TO   STNOKRF-READ-SEC
         END-IF
     END-IF.
*出荷数チェック
     IF  PARA-KBN2   =  "1"
         COMPUTE WK-GK-SYUKASU =  STN-F31 + STN-F33 +  STN-F35 +
                 STN-F37 + STN-F39
         IF  WK-GK-SYUKASU NOT = STN-F14
             CONTINUE
         ELSE
             GO              TO   STNOKRF-READ-SEC
         END-IF
     END-IF.
*
     ADD     1               TO   TAISYO-CNT.
*
 STNOKRF-READ-EXIT.
     EXIT.
****************************************************************
*    取引先マスタ読込　　　　　　　　　                        *
****************************************************************
 HTOKMS-READ-SEC       SECTION.
     MOVE     "HTOKMS-READ-SEC"   TO   S-NAME.
*
     MOVE  STN-F03                TO   TOK-F01.
     READ  HTOKMS
           INVALID        MOVE "INV"   TO  HTOKMS-INV-FLG
           NOT  INVALID   MOVE SPACE   TO  HTOKMS-INV-FLG
     END-READ.
*
 HTOKMS-READ-EXIT.
     EXIT.
****************************************************************
*    店舗マスタ読込　　　　　　　　　　                        *
****************************************************************
 HTENMS-READ-SEC        SECTION.
     MOVE     "HTENMS-READ-SEC"   TO   S-NAME.
*
     MOVE  STN-F03                TO   TEN-F52.
     MOVE  STN-F06                TO   TEN-F011.
     READ  HTENMS
           INVALID        MOVE "INV"   TO  HTENMS-INV-FLG
           NOT  INVALID   MOVE SPACE   TO  HTENMS-INV-FLG
     END-READ.
*
 HTENMS-READ-EXIT.
     EXIT.
****************************************************************
*    倉庫マスタ読込　　　　　　　　　　                        *
****************************************************************
 ZSOKMS-READ-SEC        SECTION.
     MOVE     "ZSOKMS-READ-SEC"   TO   S-NAME.
*
     MOVE  STN-F04                TO   SOK-F01.
     READ  ZSOKMS
           INVALID        MOVE "INV"   TO  ZSOKMS-INV-FLG
           NOT  INVALID   MOVE SPACE   TO  ZSOKMS-INV-FLG
     END-READ.
*
 ZSOKMS-READ-EXIT.
     EXIT.
****************************************************************
*    商品名称マスタ読込　　　　　　　　                        *
****************************************************************
 HMEIMS-READ-SEC        SECTION.
     MOVE     "HMEIMS-READ-SEC"   TO   S-NAME.
*
     MOVE  STN-F11                TO   MEI-F011.
     MOVE  STN-F12(1:5)           TO   MEI-F0121.
     MOVE  STN-F12(6:2)           TO   MEI-F0122.
     MOVE  STN-F12(8:1)           TO   MEI-F0123.
     READ  HMEIMS
           INVALID        MOVE "INV"   TO  HMEIMS-INV-FLG
           NOT  INVALID   MOVE SPACE   TO  HMEIMS-INV-FLG
     END-READ.
*
 HMEIMS-READ-EXIT.
     EXIT.
*****************<<  SST0050V   END PROGRAM  >>******************
