****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　出荷管理システム　　　　　　　　　*
*    業務名　　　　　　　：　欠品リスト　　　　　　　　        *
*    モジュール名　　　　：　オンラインデータ欠品リスト　　　　*
*    作成日／更新日　　　：　05/10/11                          *
*    作成者／更新者　　　：　ＮＡＶ松野                        *
*    処理概要　　　　　　：　商品単位で欠品リストを発行　　　　*
*                                                              *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SSY0808L.
 AUTHOR.               NAV.
 DATE-WRITTEN.         05/10/11.
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
*欠品ファイル
     SELECT  KPPFILF   ASSIGN    TO        KPPFILL2
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      SEQUENTIAL
                       RECORD    KEY       KPN-F46
                                           KPN-F47
                                           KPN-F01
                                           KPN-F48
                                           KPN-F25
                                           KPN-F02
                                           KPN-F07
                                           KPN-F112
                                           KPN-F03
                       FILE      STATUS    KPN-ST.
*取引先マスタ
     SELECT  HTOKMS    ASSIGN    TO        TOKMS2
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       TOK-F01
                       FILE      STATUS    TOK-ST.
*店舗マスタ
     SELECT  HTENMS    ASSIGN    TO        TENMS1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       TEN-F52
                                           TEN-F011
                       FILE      STATUS    TEN-ST.
*倉庫マスタ
     SELECT  ZSOKMS    ASSIGN    TO        ZSOKMS1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       SOK-F01
                       FILE      STATUS    SOK-ST.
*
*プリント定義ファイル
     SELECT     PRTFILE      ASSIGN    TO        LP-04-PRTF
                             FILE      STATUS    PRT-ST.
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = 欠品ファイル                     *
****************************************************************
 FD  KPPFILF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      KPPFILF   OF   XFDLIB
                       JOINING   KPN       AS   PREFIX.
****************************************************************
*    FILE = 取引先マスタ                                       *
****************************************************************
 FD  HTOKMS
                       BLOCK     CONTAINS  8    RECORDS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      HTOKMS    OF   XFDLIB
                       JOINING   TOK       AS   PREFIX.
****************************************************************
*  FILE= 店舗マスタ                                          *
****************************************************************
 FD  HTENMS
                       BLOCK     CONTAINS  8    RECORDS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      HTENMS    OF   XFDLIB
                       JOINING   TEN       AS   PREFIX.
****************************************************************
*  FILE= 倉庫マスタ                                          *
****************************************************************
 FD  ZSOKMS
                       BLOCK     CONTAINS  8    RECORDS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      ZSOKMS    OF   XFDLIB
                       JOINING   SOK       AS   PREFIX.
****************************************************************
*    FILE = プリントファイル                                   *
****************************************************************
 FD  PRTFILE
     LABEL       RECORD    IS        OMITTED.
 01  PRT-REC.
     03  FILLER            PIC X(200).
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  KPN-ST                   PIC  X(02).
     03  TOK-ST                   PIC  X(02).
     03  TEN-ST                   PIC  X(02).
     03  SOK-ST                   PIC  X(02).
     03  PRT-ST                   PIC  X(02).
*フラグ領域
 01  FLG-AREA.
     03  READ-FLG                 PIC  X(03)  VALUE  ZERO.
     03  ERR-FLG                  PIC  9(02)  VALUE  ZERO.
     03  END-FLG                  PIC  X(03)  VALUE  SPACE.
***  バッチ_の売上伝票Ｆへの存在チェック
     03  DEN-FLG                  PIC  9(01)  VALUE  ZERO.
 01  READ-CNT                     PIC  9(08)  VALUE  0.
 01  READ-CNT1                    PIC  9(08)  VALUE  0.
 01  WK-BATI.
     03  WK-BTDATE                PIC  9(08)  VALUE  ZERO.
     03  WK-BTTIME                PIC  9(04)  VALUE  ZERO.
     03  WK-BTTORI                PIC  9(08)  VALUE  ZERO.
     03  WK-SOKCD                 PIC  X(02)  VALUE  SPACE.
 01  WK-SHOCD                     PIC  X(13)  VALUE  SPACE.
*ワーク領域
 01  WRK-AREA.
***  明細ブレイク判定
     03  WK-DENNO                 PIC  9(09)  VALUE  ZERO.
***  倉庫ブレイク判定
     03  WK-SOKO                  PIC  X(02)  VALUE  ZERO.
***  ラインカウント
     03  L-CNT                    PIC  9(02)  VALUE  ZERO.
***  ページカウント
     03  P-CNT                    PIC  9(03)  VALUE  ZERO.
***  伝票_１枚目判定
     03  RD-SW                    PIC  9(01)  VALUE  ZERO.
***  モード退避
     03  SAV-SHORI                PIC  9(01)  VALUE  ZERO.
***  開始倉庫コード
     03  SOKCD1                   PIC  X(02)  VALUE  ZERO.
***  終了倉庫コード
     03  SOKCD2                   PIC  X(02)  VALUE  ZERO.
***  欠品数量計（伝票毎）
     03  WK-KEPPINKEI1            PIC  9(09)  VALUE  ZERO.
***  欠品総原価計（伝票毎）
     03  WK-GENKAKEI1             PIC  9(09)  VALUE  ZERO.
***  欠品総売価計（伝票毎）
     03  WK-BAIKAKEI1             PIC  9(09)  VALUE  ZERO.
***  欠品数量計（総合計）
     03  WK-KEPPINKEI2            PIC  9(09)  VALUE  ZERO.
***  欠品総原価計（総合計）
     03  WK-GENKAKEI2             PIC  9(09)  VALUE  ZERO.
***  欠品総売価計（総合計）
     03  WK-BAIKAKEI2             PIC  9(09)  VALUE  ZERO.
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
*メッセージの取得
 01  ERR-MSG-AREA.
     03  ERR-MSG1.
         05  FILLER              PIC   N(20)
             VALUE NC"バッチ_を入力して下さい".
     03  ERR-MSG2.
         05  FILLER              PIC   N(20)
             VALUE NC"取引先マスタに登録されていません".
     03  ERR-MSG3.
         05  FILLER              PIC   N(20)
             VALUE NC"無効キーです".
     03  ERR-MSG4.
         05  FILLER              PIC   N(20)
             VALUE NC"バッチ_（日付）論理エラー".
     03  ERR-MSG5.
         05  FILLER              PIC   N(20)
             VALUE NC"バッチ_（時間）論理エラー".
     03  ERR-MSG6.
         05  FILLER              PIC   N(20)
             VALUE NC"正しい値を入力してください".
     03  ERR-MSG7.
         05  FILLER              PIC   N(20)
             VALUE NC"売上伝票ファイルに登録されていません".
     03  ERR-MSG8.
         05  FILLER              PIC   N(20)
             VALUE NC"倉庫マスタに登録されていません".
     03  ERR-MSG9.
         05  FILLER              PIC   N(20)
             VALUE NC"開始納品日を正しく入力して下さい".
     03  ERR-MSG10.
         05  FILLER              PIC   N(20)
             VALUE NC"終了納品日を正しく入力して下さい".
     03  ERR-MSG11.
         05  FILLER              PIC   N(20)
             VALUE NC"開始が終了を超えています".
 01  ERR-MSG-AREA-R      REDEFINES     ERR-MSG-AREA.
     03  ERR-MSG-R   OCCURS  11  PIC   N(20).
*
 01  FILE-ERR.
     03  KPN-ERR           PIC N(20) VALUE
                        NC"欠品ファイルエラー".
     03  TOK-ERR           PIC N(20) VALUE
                        NC"取引先マスタエラー".
     03  TEN-ERR           PIC N(20) VALUE
                        NC"店舗マスタエラー".
     03  SOK-ERR           PIC N(20) VALUE
                        NC"倉庫マスタエラー".
     03  DSP-ERR           PIC N(20) VALUE
                        NC"画面ファイルエラー".
     03  PRT-ERR           PIC N(20) VALUE
                        NC"プリンタファイルエラー".
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*
****************************************************************
*    プリントエリア                                            *
****************************************************************
*--------------------------------------------------------------*
*    ヘッダ                                                    *
*--------------------------------------------------------------*
*
 01  HD1.
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  HD1-01                  PIC  X(08).
     03  FILLER                  PIC  X(31)  VALUE  SPACE.
     03  FILLER                  PIC  N(21)  VALUE
       NC"※※　欠　品　リ　ス　ト　（商品単位）※※"
                                 CHARACTER  TYPE  IS  CHR-21.
     03  FILLER                  PIC  X(21)  VALUE  SPACE.
     03  HD1-02                  PIC  9(04).
     03  FILLER                  PIC  N(01)  VALUE  NC"年"
                                 CHARACTER  TYPE  IS  CHR-2.
     03  HD1-03                  PIC  Z9.
     03  FILLER                  PIC  N(01)  VALUE  NC"月"
                                 CHARACTER  TYPE  IS  CHR-2.
     03  HD1-04                  PIC  Z9.
     03  FILLER                  PIC  N(01)  VALUE  NC"日"
                                 CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  HD1-05                  PIC  ZZ9.
     03  FILLER                  PIC  N(01)  VALUE  NC"頁"
                                 CHARACTER  TYPE  IS  CHR-2.
*
 01  HD2.
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  FILLER                  PIC  X(04)  VALUE  "ﾊﾞｯﾁ".
     03  FILLER                  PIC  N(02)  VALUE NC"_："
                                 CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  HD2-01                  PIC  9(08).
     03  FILLER                  PIC  X(01)  VALUE  "-".
     03  HD2-02                  PIC  9(04).
     03  FILLER                  PIC  X(01)  VALUE  "-".
     03  HD2-03                  PIC  9(08).
*
 01  HD3.
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  FILLER                  PIC  N(04)  VALUE NC"取引先："
                                 CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  HD3-01                  PIC  N(15)
                                 CHARACTER  TYPE  IS  CHR-2.
*
 01  HD4                         CHARACTER   TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  FILLER                  PIC  N(04)  VALUE NC"倉庫　：".
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  HD4-01                  PIC  X(02)  VALUE  ZERO.
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  HD4-02                  PIC  N(18)  VALUE  SPACE.
     03  FILLER                  PIC  X(45)  VALUE  SPACE.
     03  FILLER                  PIC  N(08)
                                 VALUE  NC"出力納品日範囲：".
     03  HD4-03                  PIC  9(08).
     03  FILLER                  PIC  N(01)  VALUE  NC"～".
     03  HD4-04                  PIC  9(08).
*
 01  HD6                         CHARACTER   TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  FILLER                  PIC  N(05)  VALUE
                                 NC"量販店商品".
     03  FILLER                  PIC  X(35)  VALUE  SPACE.
     03  FILLER                  PIC  N(02)  VALUE  NC"店舗".
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  FILLER                  PIC  N(04)  VALUE
                                 NC"伝票番号".
     03  FILLER                  PIC  X(03)  VALUE  SPACE.
     03  FILLER                  PIC  N(01)  VALUE
                                 NC"行".
     03  FILLER                  PIC  X(06)  VALUE  SPACE.
     03  FILLER                  PIC  N(04)  VALUE
                                 NC"発注数量".
     03  FILLER                  PIC  X(07)  VALUE  SPACE.
     03  FILLER                  PIC  N(04)  VALUE
                                 NC"訂正数量".
     03  FILLER                  PIC  X(08)  VALUE  SPACE.
     03  FILLER                  PIC  N(04)  VALUE
                                 NC"欠品数量".
     03  FILLER                  PIC  X(07)  VALUE  SPACE.
     03  FILLER                  PIC  N(03)  VALUE
                                 NC"原単価".
     03  FILLER                  PIC  X(04)  VALUE  SPACE.
     03  FILLER                  PIC  N(03)  VALUE
                                 NC"売単価".
*****03  FILLER                  PIC  X(02)  VALUE  SPACE.
*****03  FILLER                  PIC  N(02)  VALUE
*****                            NC"倉庫".
*
 01  SEN                         CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  N(25)  VALUE
         NC"─────────────────────────".
     03  FILLER                  PIC  N(25)  VALUE
         NC"─────────────────────────".
     03  FILLER                  PIC  N(18)  VALUE
         NC"──────────────────".
 01  SEN1.
     03  FILLER                  PIC  X(50)  VALUE
         "--------------------------------------------------".
     03  FILLER                  PIC  X(50)  VALUE
         "--------------------------------------------------".
     03  FILLER                  PIC  X(36)  VALUE
         "------------------------------------".
 01  DT2.
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  DT2-03                  PIC  X(13).
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  DT2-041                 PIC  X(15).
     03  DT2-042                 PIC  X(15).
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  DT2-11                  PIC  99999.
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  DT2-01                  PIC  9(09).
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  DT2-02                  PIC  Z9.
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  DT2-05                  PIC  --,---,--9.99.
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  DT2-061                 PIC  X(01)  VALUE  "(".
     03  DT2-062                 PIC  --,---,--9.99.
     03  DT2-063                 PIC  X(01)  VALUE  ")".
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  DT2-07                  PIC  --,---,--9.99.
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  DT2-08                  PIC  ----,--9.99.
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  DT2-09                  PIC  ----,--9.
*****03  FILLER                  PIC  X(03)  VALUE  SPACE.
*****03  DT2-10                  PIC  99.
*
 01  DT3                         CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(49)  VALUE  SPACE.
     03  FILLER                  PIC  N(06)  VALUE
                                 NC"［欠品合計］".
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  DT3-01                  PIC  ---,---,--9.
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  FILLER                  PIC  N(08)  VALUE
                                 NC"［欠品総原価計］".
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  DT3-02                  PIC  ---,---,--9.
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  FILLER                  PIC  N(08)  VALUE
                                 NC"［欠品総売価計］".
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  DT3-03                  PIC  ---,---,--9.
*
 01  DT4                         CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(48)  VALUE  SPACE.
     03  FILLER                  PIC  N(23)  VALUE
         NC"対　象　デ　ー　タ　は　存　在　し　ま　せ　ん".
*
 01  TL1                         CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(34)  VALUE  SPACE.
     03  FILLER                  PIC  N(07)  VALUE
                                 NC"［取引先合計］".
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  FILLER                  PIC  N(06)  VALUE
                                 NC"［欠品合計］".
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  TL1-01                  PIC  ---,---,--9.
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  FILLER                  PIC  N(08)  VALUE
                                 NC"［欠品総原価計］".
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  TL1-02                  PIC  ---,---,--9.
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  FILLER                  PIC  N(08)  VALUE
                                 NC"［欠品総売価計］".
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  TL1-03                  PIC  ---,---,--9.
 LINKAGE                   SECTION.
 01  PARA-NOUDT1                 PIC  9(08).
 01  PARA-NOUDT2                 PIC  9(08).
**************************************************************
 PROCEDURE             DIVISION  USING
                                        PARA-NOUDT1
                                        PARA-NOUDT2.
**************************************************************
 DECLARATIVES.
 KPN-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE KPPFILF.
     DISPLAY     KPN-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     KPN-ST    UPON      CONS.
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
 PRT-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE PRTFILE.
     DISPLAY     PRT-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     PRT-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 END  DECLARATIVES.
****************************************************************
*                   MAIN       MODULE                3.0       *
****************************************************************
 PROCESS-SEC             SECTION.
     MOVE     "PROCESS-SEC" TO   S-NAME.
     DISPLAY  "***   SSY0808L   START     ***"  UPON CONS.
*
*ワーク初期化
     MOVE      ZERO     TO    P-CNT.
     MOVE      ZERO     TO    L-CNT.
     MOVE      ZERO     TO    RD-SW.
     MOVE      SPACE    TO    READ-FLG.
*
     PERFORM   INIT-SEC.
     PERFORM   MAIN-SEC   UNTIL     READ-FLG  =   "END".
     PERFORM   END-SEC.
     STOP      RUN.
*
 PROCESS-EXIT.
     EXIT.
****************************************************************
*             ＰＲＴファイル初期処理                 4.0       *
****************************************************************
 INIT-SEC                SECTION.
     MOVE     "INIT-SEC"    TO   S-NAME.
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
     OPEN     INPUT     KPPFILF HTOKMS   HTENMS   ZSOKMS.
     OPEN     OUTPUT    PRTFILE.
*ワークの初期化
     INITIALIZE         FLG-AREA.
*
     IF       DEN-FLG  NOT =  9
              PERFORM  KPN-READ-SEC
              IF       READ-FLG  = "END"
                       PERFORM  BODY-WRITE2-SEC
                       GO   TO  INIT-EXIT
              END-IF
              PERFORM  HEAD-WRITE-SEC
*ワーク初期値入力
              MOVE     KPN-F48   TO   WK-SOKO
     ELSE
              DISPLAY NC"＃＃印字スタートエラー＃＃"  UPON  CONS
              STOP  RUN
     END-IF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*             明細出力処理２　　                     4.1       *
****************************************************************
 BODY-WRITE2-SEC        SECTION.
     MOVE     "BODY-WRITE2-SEC"    TO   S-NAME.
*
*明細部出力
     WRITE    PRT-REC      FROM   DT4       AFTER  3.
*欠品リスト出力終了
     MOVE    "END"         TO     READ-FLG.
*
 BODY-WRITE2-EXIT.
     EXIT.
****************************************************************
*             ＰＲＴファイルメイン処理               5.0       *
****************************************************************
 MAIN-SEC                SECTION.
     MOVE     "MAIN-SEC"    TO   S-NAME.
*
*改ページ判定
     IF       L-CNT     >=   60
              MOVE      SPACE    TO    PRT-REC
              WRITE     PRT-REC  AFTER PAGE
              MOVE      ZERO     TO    L-CNT
              MOVE      ZERO     TO    RD-SW
              MOVE      SPACE    TO    WK-SHOCD
              PERFORM   HEAD-WRITE-SEC
     END-IF.
*倉庫コードブレイク判定
     IF       KPN-F48   NOT = WK-SOKO
              PERFORM   TAIL-WRITE-SEC
              MOVE      SPACE    TO    PRT-REC
              WRITE     PRT-REC  AFTER PAGE
              MOVE      ZERO     TO    L-CNT
              MOVE      ZERO     TO    RD-SW
              MOVE      SPACE    TO    WK-SHOCD
              MOVE      KPN-F48  TO    WK-SOKO
              PERFORM   HEAD-WRITE-SEC
     END-IF.
*
     IF       READ-FLG  NOT = "END"
              PERFORM   BODY-WRITE1-SEC
     ELSE
              GO   TO   MAIN-EXIT
     END-IF.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*             ヘッダ部出力処理                       5.1       *
****************************************************************
 HEAD-WRITE-SEC                  SECTION.
     MOVE     "HEAD-WRITE-SEC"    TO   S-NAME.
*ページカウント
     ADD      1         TO        P-CNT.
*項目設定
*ヘッダ１
***  ＰＧＩＤ
     MOVE     "SSY0808L"          TO        HD1-01.
***  日付
     MOVE     HEN-DATE-YYYY       TO        HD1-02.
     MOVE     HEN-DATE-MM         TO        HD1-03.
     MOVE     HEN-DATE-DD         TO        HD1-04.
***  ページ_
     MOVE     P-CNT               TO        HD1-05.
*
*ヘッダ２
***  バッチ_
     MOVE     KPN-F46             TO        HD2-01.
     MOVE     KPN-F47             TO        HD2-02.
     MOVE     KPN-F01             TO        HD2-03.
*ヘッダ３
***  取引先名
     MOVE     KPN-F01             TO        TOK-F01.
     READ     HTOKMS
       INVALID
              MOVE      SPACE     TO        HD3-01
       NOT INVALID
              MOVE      TOK-F03   TO        HD3-01
     END-READ.
*ヘッダ４
***  倉庫マスタＲＥＡＤ
     MOVE      KPN-F48        TO   HD4-01   SOK-F01.
     READ      ZSOKMS
       INVALID
               MOVE   SPACE   TO   HD4-02
       NOT INVALID
               MOVE   SOK-F02 TO   HD4-02
     END-READ.
     MOVE      PARA-NOUDT1    TO   HD4-03.
     MOVE      PARA-NOUDT2    TO   HD4-04.
*ヘッダ部出力
     WRITE    PRT-REC      FROM   HD1       AFTER  2.
     WRITE    PRT-REC      FROM   HD2       AFTER  2.
     WRITE    PRT-REC      FROM   HD3       AFTER  1.
     WRITE    PRT-REC      FROM   HD4       AFTER  1.
     WRITE    PRT-REC      FROM   SEN       AFTER  1.
     WRITE    PRT-REC      FROM   HD6       AFTER  1.
     WRITE    PRT-REC      FROM   SEN       AFTER  1.
*
     ADD      9            TO        L-CNT.
 HEAD-WRITE-EXIT.
     EXIT.
****************************************************************
*             明細部出力処理１                       5.2       *
****************************************************************
 BODY-WRITE1-SEC              SECTION.
     MOVE     "BODY-WT-SEC"       TO   S-NAME.
*
     MOVE     SPACE     TO        DT2.
*
*明細部出力
***    商品_ブレイク時，合計及び破線表示
     IF       KPN-F25   NOT =     WK-SHOCD
              IF   RD-SW   =   ZERO
                   MOVE      1    TO   RD-SW
                   MOVE      KPN-F25   TO   WK-SHOCD
              ELSE
***              合計欄出力
                   PERFORM   DEN-GOKEI-SEC
***              破線出力
                   WRITE     PRT-REC   FROM SEN1 AFTER 1
***
                   MOVE      KPN-F25   TO   WK-SHOCD
                   ADD       1         TO   L-CNT
              END-IF
*    店舗，伝票_の重複表示不可
***  量販店商品
              MOVE     KPN-F25             TO   DT2-03
***  商品名１
              MOVE     KPN-F1421           TO   DT2-041
***  商品名２
              MOVE     KPN-F1422           TO   DT2-042
     END-IF.
***  店舗CD
     MOVE     KPN-F07             TO   DT2-11.
***  伝票_
     MOVE     KPN-F02             TO   DT2-01.
***  行
     MOVE     KPN-F03             TO   DT2-02.
***  発注数量
     MOVE     KPN-F50             TO   DT2-05.
***  訂正数量
     MOVE     "("                 TO   DT2-061.
     MOVE     KPN-F15             TO   DT2-062.
     MOVE     ")"                 TO   DT2-063.
***  欠品数量
     COMPUTE  DT2-07 = KPN-F50 - KPN-F15.
***  原単価
     MOVE     KPN-F172            TO   DT2-08.
***  売単価
     MOVE     KPN-F173            TO   DT2-09.
***  倉庫コード
***  MOVE     KPN-F48             TO   DT2-10.
*合計
***伝票毎
***  欠品数量合計
     COMPUTE  WK-KEPPINKEI1 =  ( KPN-F50 - KPN-F15 )
                                  +  WK-KEPPINKEI1
***  欠品総原価計
     COMPUTE  WK-GENKAKEI1  = (( KPN-F50 - KPN-F15 ) * KPN-F512 )
                                  + WK-GENKAKEI1
***  欠品総売価計
     COMPUTE  WK-BAIKAKEI1  = (( KPN-F50 - KPN-F15 ) * KPN-F513 )
                                  + WK-BAIKAKEI1
***総合計
***  欠品数量合計
     COMPUTE  WK-KEPPINKEI2 =  ( KPN-F50 - KPN-F15 )
                                  +  WK-KEPPINKEI2
***  欠品総原価計
     COMPUTE  WK-GENKAKEI2  = (( KPN-F50 - KPN-F15 ) * KPN-F512 )
                                  + WK-GENKAKEI2
***  欠品総売価計
     COMPUTE  WK-BAIKAKEI2  = (( KPN-F50 - KPN-F15 ) * KPN-F513 )
                                  + WK-BAIKAKEI2
*伝票２行目出力
     WRITE    PRT-REC      FROM   DT2       AFTER  1.
*
     ADD      1    TO   L-CNT.

     PERFORM  KPN-READ-SEC.
     IF        READ-FLG    = "END"
               PERFORM     TAIL-WRITE-SEC
               GO   TO     BODY-WRITE1-EXIT
     END-IF.
*６０行以上で，かつ商品_ブレイク時，合計行出力
     IF       L-CNT >= 60
        AND   KPN-F25   NOT =  WK-SHOCD
              PERFORM   DEN-GOKEI-SEC
     END-IF.
*
 BODY-WRITE1-EXIT.
     EXIT.
****************************************************************
*             伝票毎合計欄出力処理                   5.2.1     *
****************************************************************
 DEN-GOKEI-SEC         SECTION.
     MOVE     "DEN-GOKEI-SEC"     TO   S-NAME.
*
***  合計欄出力
     MOVE      WK-KEPPINKEI1  TO   DT3-01.
     MOVE      WK-GENKAKEI1   TO   DT3-02.
     MOVE      WK-BAIKAKEI1   TO   DT3-03.
     WRITE     PRT-REC   FROM DT3  AFTER 2.
***  合計初期化
     MOVE      ZERO      TO   WK-KEPPINKEI1
                              WK-GENKAKEI1
                              WK-BAIKAKEI1.
*
     ADD       2         TO   L-CNT.
*
 DEN-GOKEI-EXIT.
     EXIT.
****************************************************************
*             テール部出力処理                       5.3       *
****************************************************************
 TAIL-WRITE-SEC        SECTION.
     MOVE     "TAIL-WRITE-SEC"    TO   S-NAME.
*
*最後の伝票毎合計を出力
     PERFORM   DEN-GOKEI-SEC.
*総合計出力
     MOVE      WK-KEPPINKEI2       TO    TL1-01.
     MOVE      WK-GENKAKEI2        TO    TL1-02.
     MOVE      WK-BAIKAKEI2        TO    TL1-03.
*
     WRITE     PRT-REC   FROM      TL1   AFTER  1.
*
     MOVE     ZERO TO   WK-KEPPINKEI2
                        WK-GENKAKEI2
                        WK-BAIKAKEI2.
 TAIL-WRITE-EXIT.
     EXIT.
****************************************************************
*             　欠品ファイルＲＥＡＤ　                       *
****************************************************************
 KPN-READ-SEC          SECTION.
     MOVE     "KPN-READ-SEC"      TO   S-NAME.
*
     READ     KPPFILF       AT    END
              MOVE         "END"  TO   READ-FLG
              GO   TO   KPN-READ-EXIT
         NOT  AT   END
              ADD        1  TO    READ-CNT1
     END-READ.
     IF       READ-CNT      =     0
              MOVE    KPN-F46     TO   WK-BTDATE
              MOVE    KPN-F47     TO   WK-BTTIME
              MOVE    KPN-F01     TO   WK-BTTORI
              MOVE    KPN-F48     TO   WK-SOKCD
     END-IF.
*終了判定
     IF       KPN-F46   NOT =     WK-BTDATE
          OR  KPN-F47   NOT =     WK-BTTIME
          OR  KPN-F01   NOT =     WK-BTTORI
**********OR  KPN-F48       >     WK-SOKCD
**********OR  DEN-F48       >     DSP-SOKCD2
              MOVE     "END"      TO   READ-FLG
              GO   TO   KPN-READ-EXIT
     END-IF.
*訂正数量と発注数量が同じ物は読み飛ばし
     IF       KPN-F50   =   KPN-F15
              GO   TO   KPN-READ-SEC
     END-IF.
     ADD      1    TO   READ-CNT.
*
 KPN-READ-EXIT.
     EXIT.
****************************************************************
*             終了処理                               6.0       *
****************************************************************
 END-SEC               SECTION.
*ファイル ＣＬＯＳＥ
     CLOSE             KPPFILF  HTOKMS  HTENMS  PRTFILE
                       ZSOKMS.
     DISPLAY  "***   READ-CNT = " READ-CNT1 " *** " UPON CONS.
     DISPLAY  "***   KPN -CNT = " READ-CNT  " *** " UPON CONS.
     DISPLAY  "***   SSY0808L   END       ***"  UPON CONS.
**
 END-EXIT.
     EXIT.
*****************<<  SSY0801L   END PROGRAM  >>******************
