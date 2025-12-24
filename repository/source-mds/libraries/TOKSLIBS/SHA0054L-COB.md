# SHA0054L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SHA0054L.COB`

## ソースコード

```cobol
**********************************************************
*                                                        *
*    顧客名　　　　　　　：　（株）サカタのタネ殿        *
*    業務名　　　　　　　：　在庫管理システム            *
*    モジュール名　　　　：　発注書発行                  *
*    作成日／更新日　　　：　14/05/16                    *
*    作成者／更新者　　　：　ＮＡＶ高橋                  *
*                            （ヘッダＬ５キー使用）      *
**********************************************************
 IDENTIFICATION            DIVISION.
 PROGRAM-ID.               SHA0054L.
 AUTHOR.                   N.T.
 DATE-WRITTEN.             14/05/16.
**********************************************************
 ENVIRONMENT               DIVISION.
**********************************************************
 CONFIGURATION             SECTION.
 SOURCE-COMPUTER.
 OBJECT-COMPUTER.
 SPECIAL-NAMES.
     STATION     IS        STA
     CONSOLE     IS        CONS.
***********************************************************
*             INPUT-OUTPUT                                *
***********************************************************
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*発注ファイル（ヘッダ）
     SELECT      HACHEDF   ASSIGN    TO        DA-01-VI-HACHEDL5
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      DYNAMIC
                           RECORD    KEY       HED-F40
                                               HED-F02
                           FILE      STATUS    HED-ST.
*発注ファイル（明細）
     SELECT      HACMEIF   ASSIGN    TO        DA-01-VI-HACMEIL1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      DYNAMIC
                           RECORD    KEY       BDY-F02
                                               BDY-F03
                           FILE      STATUS    BDY-ST.
*得意先マスタ
     SELECT      HTOKMS    ASSIGN    TO        DA-01-VI-TOKMS2
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       TOK-F01
                           FILE      STATUS    TOK-ST.
*仕入先マスタ
     SELECT      ZSHIMS    ASSIGN    TO        DA-01-VI-ZSHIMS1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       SHI-F01
                           FILE      STATUS    SHI-ST.
*商品名称マスタ
     SELECT      HMEIMS    ASSIGN    TO        DA-01-VI-MEIMS1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       MEI-F011  MEI-F012
                           FILE      STATUS    MEI-ST.
*倉庫マスタ
     SELECT      ZSOKMS    ASSIGN    TO        DA-01-VI-ZSOKMS1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       SOK-F01
                           FILE      STATUS    SOK-ST.
*店舗マスタ
     SELECT      HTENMS    ASSIGN    TO        DA-01-VI-TENMS1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       TEN-F52  TEN-F011
                           FILE      STATUS    TEN-ST.
*条件ファイル
     SELECT      HJYOKEN   ASSIGN    TO        DA-01-VI-JYOKEN1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       JYO-F01   JYO-F02
                           FILE      STATUS    JYO-ST.
*%* プリンター *%*
     SELECT     PRTF       ASSIGN    TO        GS-XU04LP
                           DESTINATION        "PRT"
                           FORMAT              PRT-FORM
                           GROUP               PRT-GRP
                           PROCESSING          PRT-PROC
                           CONTROL             PRT-CONTROL
                           FILE      STATUS    PRT-ST.
*%* 表示ファイル *%*
     SELECT     DSPF       ASSIGN    TO        GS-DSPF
                           ORGANIZATION        SEQUENTIAL
                           DESTINATION        "DSP"
                           FORMAT              DSP-FORM
                           GROUP               DSP-GRP
                           PROCESSING          DSP-PROC
                           FUNCTION            DSP-FNC
                           CONTROL             DSP-CONTROL
                           STATUS              DSP-ST.
******************************************************************
*             DATA                DIVISION                       *
******************************************************************
 DATA                      DIVISION.
 FILE                      SECTION.
 FD  HACHEDF.
     COPY        HACHEDF   OF        XFDLIB
                 JOINING   HED   AS  PREFIX.
 FD  HACMEIF.
     COPY        HACMEIF   OF        XFDLIB
                 JOINING   BDY   AS  PREFIX.
 FD  HTOKMS.
     COPY        HTOKMS    OF        XFDLIB
                 JOINING   TOK   AS  PREFIX.
 FD  ZSHIMS.
     COPY        ZSHIMS    OF        XFDLIB
                 JOINING   SHI   AS  PREFIX.
 FD  HMEIMS.
     COPY        HMEIMS    OF        XFDLIB
                 JOINING   MEI   AS  PREFIX.
 FD  ZSOKMS.
     COPY        ZSOKMS    OF        XFDLIB
                 JOINING   SOK   AS  PREFIX.
 FD  HTENMS.
     COPY        HTENMS    OF        XFDLIB
                 JOINING   TEN   AS  PREFIX.
 FD  HJYOKEN.
     COPY        HJYOKEN   OF        XFDLIB
                 JOINING   JYO   AS  PREFIX.
 FD  PRTF.
     COPY        FHA00513  OF        XMDLIB
                 JOINING   PRT   AS  PREFIX.
 FD  DSPF.
     COPY        FHA00511  OF        XMDLIB
                 JOINING   DSP   AS  PREFIX.
******************************************************************
 WORKING-STORAGE       SECTION.
******************************************************************
*%*表示パラメータ*%*
 01  DSP-AREA.
     03  DSP-FORM          PIC X(08).
     03  DSP-PROC          PIC X(02).
     03  DSP-GRP           PIC X(08).
     03  DSP-FNC           PIC X(04).
     03  DSP-CONTROL.
         05  DSP-CNTRL     PIC X(04).
         05  DSP-STR-PG    PIC X(02).
 01  PRT-AREA.
     03  PRT-FORM          PIC X(08).
     03  PRT-PROC          PIC X(02).
     03  PRT-GRP           PIC X(08).
     03  PRT-CONTROL.
         05  PRT-CNTRL     PIC X(04).
         05  PRT-STR-PG    PIC X(02).
*
 01  FILE-STATUS.
     03  HED-ST            PIC X(02).
     03  BDY-ST            PIC X(02).
     03  TOK-ST            PIC X(02).
     03  SHI-ST            PIC X(02).
     03  MEI-ST            PIC X(02).
     03  SOK-ST            PIC X(02).
     03  TEN-ST            PIC X(02).
     03  JYO-ST            PIC X(02).
     03  PRT-ST            PIC X(02).
     03  DSP-ST            PIC X(02).
 01  IN-DATA               PIC X(01).
 01  JEF-AREA.
     03  JEF-F06.
         05  JEF-F061      PIC N(03).
         05  JEF-F062      PIC N(04).
     03  JEF-F07           PIC X(01).
 01  WK-JEF.
     03  WK-JEF01          PIC N(03).
     03  WK-JEF02          PIC N(01).
     03  WK-JEF03          PIC N(04).
*日付／時刻
 01  SYS-DATE                PIC  9(08).
 01  FILLER                  REDEFINES   SYS-DATE.
     03  FILLER              PIC  9(02).
     03  SYS-YMD.
       05  SYS-YY            PIC  9(02).
       05  SYS-MM            PIC  9(02).
       05  SYS-DD            PIC  9(02).
 01  SYS-TIME.
     03  SYS-HH              PIC  9(02).
     03  SYS-MN              PIC  9(02).
     03  SYS-SS              PIC  9(02).
     03  FILLER              PIC  9(02).
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
*
****　サブルーチン用連絡領域（日本語変換の為）****
 01  EBCC-RTNCD          PIC S9(09)  BINARY.
 01  EBCC-INLT           PIC S9(04)  BINARY  VALUE  7.
 01  EBCC-INDT           PIC  X(07).
 01  EBCC-OTLT           PIC S9(04)  BINARY  VALUE 14.
 01  EBCC-OTDT           PIC  N(07).
 01  EBCC-DTLT           PIC S9(04)  BINARY.
*
 01  WK-AREA.
     03  IX                PIC 9(02).
     03  WK-DENNO          PIC 9(07).
     03  WK-GOKEI          PIC 9(10).
     03  WK-SURYO          PIC 9(08)V99.
     03  WK-SURYOR  REDEFINES  WK-SURYO.
         05  WK-SURYOA     PIC 9(08).
         05  WK-SURYOB     PIC 9(02).
     03  WK-SITAN          PIC 9(07)V99.
     03  WK-SITANR  REDEFINES  WK-SITAN.
         05  WK-SITANA     PIC 9(07).
         05  WK-SITANB     PIC 9(02).
     03  WK-BATAN          PIC 9(07)V99.
     03  WK-BATANR  REDEFINES  WK-BATAN.
         05  WK-BATANA     PIC 9(07).
         05  WK-BATANB     PIC 9(02).
     03  WK-BDY-KEY.
         05  WK-BDY-F02    PIC 9(07).
*フラグ
 01  FLG-AREA.
     03  END-FLG           PIC X(03)   VALUE  SPACE.
     03  SHORI-FLG         PIC 9(01)   VALUE  ZERO.
     03  ERR-FLG           PIC X(03)   VALUE  SPACE.
*****03  HAC-FLG           PIC 9(01)   VALUE  ZERO.
*システム日付
 01  WK-SYSYMD             PIC 9(06).
 01  WK-SYSYMDR   REDEFINES WK-SYSYMD.
     03  WK-SYSYY          PIC 9(02).
     03  WK-SYSMM          PIC 9(02).
     03  WK-SYSDD          PIC 9(02).
 01  FILE-ERR.
     03  HED-ERR           PIC N(10) VALUE
                        NC"発注（ヘッダ）エラー".
     03  BDY-ERR           PIC N(10) VALUE
                        NC"発注（明細）エラー".
     03  TOK-ERR           PIC N(10) VALUE
                        NC"取引先マスタエラー".
     03  SHI-ERR           PIC N(10) VALUE
                        NC"仕入先マスタエラー".
     03  MEI-ERR           PIC  N(10) VALUE
                        NC"商品名称マスタエラー".
     03  SOK-ERR           PIC N(10) VALUE
                        NC"倉庫マスタエラー".
     03  TEN-ERR           PIC N(10) VALUE
                        NC"店舗マスタエラー".
     03  JYO-ERR           PIC N(10) VALUE
                        NC"条件ファイルエラー".
     03  PRT-ERR           PIC N(10) VALUE
                        NC"プリンタＦエラー".
     03  DSP-ERR           PIC  N(10) VALUE
                        NC"画面ファイルエラー".
*エラーメッセージ
 01  MSG-AREA.
     03  MSG01             PIC  N(20)  VALUE
           NC"ＰＦキーエラーです！".
     03  MSG02             PIC  N(20)  VALUE
           NC"区分エラーです！".
     03  MSG03             PIC  N(20)  VALUE
           NC"開始が終了を超えています！".
     03  MSG04             PIC  N(20)  VALUE
           NC"対象データがありません！".
     03  MSG05             PIC  N(20)  VALUE
           NC"単価出力に誤りがあります".
     03  MSG99             PIC  N(20)  VALUE
           NC"『リスト出力中です！』".
*ＰＦメッセージ
 01  PF-AREA.
     03  PFKEY01           PIC  N(20)  VALUE
           NC"_取消　_終了".
     03  PFKEY02           PIC  N(20)  VALUE
           NC"_取消　_終了　_項目戻り".
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
 LINKAGE                   SECTION.
 01  LINK-MAIN-AREA.
     03  LNK-M-TAN         PIC X(02).
     03  LNK-M-BUMON       PIC X(04).
******************************************************************
*             PROCEDURE           DIVISION                       *
******************************************************************
 PROCEDURE                 DIVISION  USING  LINK-MAIN-AREA.
 DECLARATIVES.
 HED-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HACHEDF.
     DISPLAY     HED-ERR   UPON      STA.
     DISPLAY     HED-ST    UPON      STA.
     STOP        RUN.
 BDY-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HACMEIF.
     DISPLAY     BDY-ERR   UPON      STA.
     DISPLAY     BDY-ST    UPON      STA.
     STOP        RUN.
 TOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HTOKMS.
     DISPLAY     TOK-ERR   UPON      STA.
     DISPLAY     TOK-ST    UPON      STA.
     STOP        RUN.
 SHI-ERR                  SECTION.
     USE         AFTER     EXCEPTION PROCEDURE ZSHIMS.
     DISPLAY     SHI-ERR   UPON      STA.
     DISPLAY     SHI-ST    UPON      STA.
     STOP        RUN.
 MEI-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HMEIMS.
     DISPLAY     MEI-ERR   UPON      STA.
     DISPLAY     MEI-ST    UPON      STA.
     STOP        RUN.
 SOK-ERR                  SECTION.
     USE         AFTER     EXCEPTION PROCEDURE ZSOKMS.
     DISPLAY     SOK-ERR   UPON      STA.
     DISPLAY     SOK-ST    UPON      STA.
     STOP        RUN.
 TEN-ERR                  SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HTENMS.
     DISPLAY     TEN-ERR   UPON      STA.
     DISPLAY     TEN-ST    UPON      STA.
     STOP        RUN.
 JYO-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HJYOKEN.
     DISPLAY     JYO-ERR   UPON      STA.
     DISPLAY     JYO-ST    UPON      STA.
     STOP        RUN.
 PRT-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE PRTF.
     DISPLAY     PRT-ERR   UPON      STA.
     DISPLAY     PRT-ST    UPON      STA.
     STOP        RUN.
 DSP-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE DSPF.
     DISPLAY     DSP-ERR   UPON      STA.
     DISPLAY     DSP-ST    UPON      STA.
     STOP        RUN.
 END DECLARATIVES.
***********************************************************
*                     ＭＡＩＮ処理                        *
***********************************************************
 PROC-SEC                  SECTION.
     DISPLAY  "**  SHA0054L   START  **"   UPON  CONS.
*
     PERFORM     INIT-SEC.
     PERFORM     MAIN-SEC  UNTIL     END-FLG  =  "END".
     PERFORM     END-SEC.
*
     DISPLAY  "**  SHA0054L    END   **"   UPON  CONS.
     STOP        RUN.
 PROC-EXIT.
     EXIT.
**********************************************************
*                      Ｉ Ｎ Ｉ Ｔ                       *
**********************************************************
 INIT-SEC                  SECTION.
*各ファイルのＯＰＥＮ
     OPEN    I-O       HACHEDF.
     OPEN    INPUT     HACMEIF.
     OPEN    INPUT     HTOKMS.
     OPEN    INPUT     ZSHIMS.
     OPEN    INPUT     HMEIMS.
     OPEN    INPUT     ZSOKMS.
     OPEN    INPUT     HTENMS.
     OPEN    INPUT     HJYOKEN.
     OPEN    OUTPUT    PRTF.
     OPEN    I-O       DSPF.
*初期値設定
     ACCEPT  WK-SYSYMD           FROM      DATE.
     INITIALIZE   WK-AREA.
     MOVE    SPACE               TO        END-FLG.
     MOVE    SPACE               TO        DSP-AREA.
     MOVE    SPACE               TO        PRT-AREA.
     MOVE    1                   TO        SHORI-FLG.
*
     ACCEPT      SYS-YMD     FROM  DATE.
     ACCEPT      SYS-TIME    FROM  TIME.
*システム日付・時刻の取得
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     SYS-YMD             TO   LINK-IN-YMD6.
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
*画面表示時刻編集
     MOVE      SYS-TIME(1:2)      TO   HEN-TIME-HH.
     MOVE      SYS-TIME(3:2)      TO   HEN-TIME-MM.
     MOVE      SYS-TIME(5:2)      TO   HEN-TIME-SS.
*特販部名称編集
     MOVE    SPACE               TO        JYO-REC.
     INITIALIZE                            JYO-REC.
     MOVE    "99"                TO        JYO-F01.
     MOVE    LNK-M-BUMON         TO        JYO-F02.
     READ    HJYOKEN
       INVALID KEY
             MOVE NC"＊＊＊＊＊＊"   TO    HEN-TOKHAN
       NOT INVALID KEY
             MOVE JYO-F03            TO    HEN-TOKHAN
     END-READ.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*    MAIN-SEC                                                  *
****************************************************************
 MAIN-SEC          SECTION.
     EVALUATE SHORI-FLG
         WHEN     1          PERFORM   DSP-INIT-SEC
         WHEN     2          PERFORM   KUBUN-SEC
         WHEN     3          PERFORM   HANI-SEC
         WHEN     4          PERFORM   KAKU-SEC
         WHEN     5          PERFORM   LIST-SEC
         WHEN     6          PERFORM   TANK-SEC
         WHEN     9          PERFORM   SAISI-SEC
     END-EVALUATE.
 MAIN-EXIT.
     EXIT.
****************************************************************
*             リスト出力                                 *
****************************************************************
 LIST-SEC          SECTION.
*発注書出力フラグの転送
     IF  (HED-F97   =   ZERO)
         PERFORM HED-RW-SEC
     END-IF.
*仕入先情報
     PERFORM SHIRE-SEC.
*納入先情報
     PERFORM NOUNYU-SEC.
*伝票情報
     PERFORM DENPYO-SEC.
*自社情報
     PERFORM JISHA-SEC.
*取引先情報
     PERFORM TORIHIK-SEC.
*発注ファイル（明細）検索
     PERFORM BDY-ST-SEC.
*明細情報
     PERFORM    VARYING   IX   FROM   1   BY   1
         UNTIL  WK-DENNO  NOT =  WK-BDY-KEY   OR  IX  >  6
            OR  ERR-FLG   NOT =  SPACE
             PERFORM   MEISAI-SEC
             PERFORM   BDY-RD-SEC
     END-PERFORM.
*合計情報
     PERFORM GOKEI-SEC.
*伝票印刷
     PERFORM DEN-WT-SEC.
*発注ファイル（ヘッダ）検索
     PERFORM HED-RD-SEC.
*
     IF  (ERR-FLG    =    "ERR")
         MOVE     9                  TO        SHORI-FLG
     END-IF.
 LIST-EXIT.
     EXIT.
***********************************************************
*            仕入先情報　　　転送
***********************************************************
 SHIRE-SEC                 SECTION.
     MOVE    SPACE               TO        PRT-FHA00513.
     MOVE    HED-F02             TO        WK-DENNO.
*項目転送
     MOVE    HED-F01             TO        PRT-DENKB.
     MOVE    "("                 TO        PRT-SSIRCD(1:1).
     MOVE    ")"                 TO        PRT-SSIRCD(10:1).
     MOVE    HED-F06             TO        PRT-SSIRCD(2:8).
*仕入先ＭのＲＥＡＤ
     MOVE    HED-F06             TO        SHI-F01.
     READ    ZSHIMS
       INVALID KEY
             GO                  TO        SHIRE-EXIT
     END-READ.
*郵便番号（ＪＥＦ変換）
     MOVE    SPACE               TO        WK-JEF.
     IF   (SHI-F121   NOT =   SPACE)
          MOVE    SHI-F121       TO        EBCC-INDT(1:3)
          MOVE    SHI-F122       TO        EBCC-INDT(4:4)
          CALL   "JCVEBCC"   USING    EBCC-RTNCD
                                      EBCC-INLT
                                      EBCC-INDT
                                      EBCC-OTLT
                                      EBCC-OTDT
                                      EBCC-DTLT
          IF   EBCC-RTNCD   NOT =   ZERO
           DISPLAY "日本語変換エラー！！" EBCC-RTNCD UPON STA
           MOVE    SPACE           TO   EBCC-OTDT
          END-IF
          MOVE    EBCC-OTDT      TO        JEF-F06
*
          MOVE    JEF-F061       TO        WK-JEF01
          MOVE    NC"－"         TO        WK-JEF02
          MOVE    JEF-F062       TO        WK-JEF03
     END-IF.
     MOVE    WK-JEF              TO        PRT-SYUBIN.
     MOVE    SHI-F07             TO        PRT-SJYUS1.
     MOVE    SHI-F08             TO        PRT-SJYUS2.
     MOVE    SHI-F02             TO        PRT-SSIRNM.
     MOVE    SHI-F04             TO        PRT-SBUKNM.
 SHIRE-EXIT.
     EXIT.
***********************************************************
*            納入先情報　　　転送
***********************************************************
 NOUNYU-SEC                 SECTION.
*項目転送の検索
     EVALUATE  HED-F01
        WHEN   50
             IF   (HED-F16    =    0)
                   PERFORM  NOU-TEN1-SEC
             ELSE
                   PERFORM  NOU-TEN5-SEC
             END-IF
        WHEN   60
*************IF   (HAC-FLG    =    0)
             IF    HED-F27  NOT =  SPACE
                  PERFORM  NOU-TEN2-SEC
             ELSE
                  IF   (HED-F16    =    0)
                       PERFORM  NOU-TEN4-SEC
                  ELSE
                       PERFORM  NOU-TEN5-SEC
                  END-IF
             END-IF
        WHEN   70
        WHEN   80
*************IF   (HAC-FLG    =    0)
             IF    HED-F27  NOT =  SPACE
                  PERFORM  NOU-TEN2-SEC
             ELSE
                  PERFORM  NOU-TEN3-SEC
             END-IF
     END-EVALUATE.
 NOUNYU-EXIT.
     EXIT.
***********************************************************
*            伝票区分 ＝ ５０
***********************************************************
 NOU-TEN1-SEC              SECTION.
*項目転送
     MOVE    HED-F17             TO        PRT-NNOUCD(2:2).
     MOVE    "("                 TO        PRT-NNOUCD(1:1).
     MOVE    ")"                 TO        PRT-NNOUCD(10:1).
*倉庫ＭのＲＥＡＤ
     MOVE    HED-F17             TO        SOK-F01.
     READ    ZSOKMS
       INVALID KEY
             GO                  TO        NOU-TEN1-EXIT
     END-READ.
     IF      SOK-F111   NOT =   SPACE
             MOVE   SOK-F111     TO        PRT-NYUBIN(1:3)
             MOVE   "-"          TO        PRT-NYUBIN(4:1)
             MOVE   SOK-F112     TO        PRT-NYUBIN(5:4)
     END-IF.
     MOVE    SOK-F05             TO        PRT-NJYUS1.
     MOVE    SOK-F06             TO        PRT-NJYUS2.
     MOVE    SOK-F02             TO        PRT-NNOUNM.
     MOVE    SOK-F07             TO        PRT-NTELNO.
 NOU-TEN1-EXIT.
     EXIT.
***********************************************************
*            伝票区分 ＝ ６０，７０，８０
*            行_　　 ＝ ０
***********************************************************
 NOU-TEN2-SEC              SECTION.
*項目転送
     MOVE    HED-F07             TO        PRT-NNOUCD(2:8).
     MOVE    "("                 TO        PRT-NNOUCD(1:1).
     MOVE    ")"                 TO        PRT-NNOUCD(10:1).
     MOVE    HED-F26             TO        PRT-NYUBIN.
     MOVE    HED-F27             TO        PRT-NJYUS1.
     MOVE    HED-F28             TO        PRT-NJYUS2.
     MOVE    HED-F29             TO        PRT-NNOUNM.
     MOVE    HED-F15             TO        PRT-NTELNO.
 NOU-TEN2-EXIT.
     EXIT.
***********************************************************
*            伝票区分    ＝ ７０，８０
*            行_    NOT ＝ ０
***********************************************************
 NOU-TEN3-SEC              SECTION.
*項目転送
     MOVE    HED-F07             TO        PRT-NNOUCD(2:8).
     MOVE    "("                 TO        PRT-NNOUCD(1:1).
     MOVE    ")"                 TO        PRT-NNOUCD(10:1).
*倉庫ＭのＲＥＡＤ
     MOVE    HED-F18             TO        SOK-F01.
     READ    ZSOKMS
       INVALID KEY
             GO                  TO        NOU-TEN3-EXIT
     END-READ.
     IF      SOK-F111   NOT =   SPACE
             MOVE   SOK-F111     TO        PRT-NYUBIN(1:3)
             MOVE   "-"          TO        PRT-NYUBIN(4:1)
             MOVE   SOK-F112     TO        PRT-NYUBIN(5:4)
     END-IF.
     MOVE    SOK-F05             TO        PRT-NJYUS1.
     MOVE    SOK-F06             TO        PRT-NJYUS2.
     MOVE    SOK-F02             TO        PRT-NNOUNM.
     MOVE    SOK-F07             TO        PRT-NTELNO.
 NOU-TEN3-EXIT.
     EXIT.
***********************************************************
*            伝票区分    ＝ ６０
*            行_    NOT ＝ ０
*            納入先　　  ＝ ０
***********************************************************
 NOU-TEN4-SEC              SECTION.
*項目転送
     MOVE    HED-F07             TO        PRT-NNOUCD(2:8).
     MOVE    "("                 TO        PRT-NNOUCD(1:1).
     MOVE    ")"                 TO        PRT-NNOUCD(10:1).
*取引先ＭのＲＥＡＤ
     MOVE    HED-F07             TO        TOK-F01.
     READ    HTOKMS
       INVALID KEY
             GO                  TO        NOU-TEN4-EXIT
     END-READ.
     IF      TOK-F801   NOT =   SPACE
             MOVE   TOK-F801     TO        PRT-NYUBIN(1:3)
             MOVE   "-"          TO        PRT-NYUBIN(4:1)
             MOVE   TOK-F802     TO        PRT-NYUBIN(5:4)
     END-IF.
     MOVE    TOK-F06             TO        PRT-NJYUS1.
     MOVE    TOK-F07             TO        PRT-NJYUS2.
     MOVE    TOK-F02             TO        PRT-NNOUNM.
     MOVE    TOK-F08             TO        PRT-NTELNO.
 NOU-TEN4-EXIT.
     EXIT.
***********************************************************
*            伝票区分    ＝ ６０
*            行_    NOT ＝ ０
*            納入先　NOT ＝ ０
***********************************************************
 NOU-TEN5-SEC              SECTION.
*項目転送
     MOVE    HED-F07             TO        PRT-NNOUCD(2:8).
     MOVE    "("                 TO        PRT-NNOUCD(1:1).
     MOVE    ")"                 TO        PRT-NNOUCD(10:1).
*店舗ＭのＲＥＡＤ
     MOVE    HED-F07             TO        TEN-F52.
     MOVE    HED-F16             TO        TEN-F011.
     READ    HTENMS
       INVALID KEY
             GO                  TO        NOU-TEN5-EXIT
     END-READ.
     IF      TEN-F781   NOT =   SPACE
             MOVE  TEN-F781      TO        PRT-NYUBIN(1:3)
             MOVE  "-"           TO        PRT-NYUBIN(4:1)
             MOVE  TEN-F782      TO        PRT-NYUBIN(5:4)
     END-IF.
     MOVE    TEN-F06             TO        PRT-NJYUS1.
     MOVE    TEN-F07             TO        PRT-NJYUS2.
     MOVE    TEN-F02             TO        PRT-NNOUNM.
     MOVE    TEN-F08             TO        PRT-NTELNO.
 NOU-TEN5-EXIT.
     EXIT.
***********************************************************
*            伝票情報　　　転送
***********************************************************
 DENPYO-SEC                 SECTION.
*項目転送
*条件ＦのＲＥＡＤ
     MOVE    "01"                TO        JYO-F01.
     MOVE    HED-F01             TO        JYO-F02.
     READ    HJYOKEN
       INVALID KEY
             MOVE SPACE          TO        PRT-DENKNM
       NOT INVALID KEY
             MOVE JYO-F03        TO        PRT-DENKNM
     END-READ.
     MOVE    HED-F02             TO        PRT-DENNO.
     MOVE    HED-F10             TO        PRT-HACDT.
     IF  (HED-F09    NOT =    ZERO)
         MOVE    HED-F09         TO        PRT-RYONO
     END-IF.
     MOVE    HED-F11             TO        PRT-NOUDT.
     IF  (HED-F19    NOT =    ZERO)
         MOVE    HED-F19         TO        PRT-SORYKB
     END-IF.
     MOVE    HED-F15             TO        PRT-MEMONO.
*＊合計情報の転送＊＊
*担当者コード
     MOVE    HED-F05             TO        PRT-TANCD.
*摘要
     MOVE    HED-F321            TO        PRT-TEKCD.
     MOVE    HED-F322            TO        PRT-TEKNM1.
     MOVE    HED-F323            TO        PRT-TEKNM2.
*部門ＣＤ
     MOVE    LNK-M-BUMON         TO        PRT-BMNCD.
*
 DENPYO-EXIT.
     EXIT.
***********************************************************
*            自社情報　　　転送
***********************************************************
 JISHA-SEC                 SECTION.
*項目転送
     MOVE    WK-SYSYY            TO        PRT-SYSYY.
     MOVE    WK-SYSMM            TO        PRT-SYSMM.
     MOVE    WK-SYSDD            TO        PRT-SYSDD.
*仕入先Ｍの検索
*****MOVE    "32299999"          TO        SHI-F01.
     IF  LNK-M-BUMON  =  "2920"
             MOVE  "29299999"    TO        SHI-F01
     ELSE
             MOVE  "29499999"    TO        SHI-F01
     END-IF.
     READ    ZSHIMS
       INVALID KEY
             GO                  TO        JISHA-EXIT
     END-READ.
     MOVE    SHI-F121            TO        PRT-TYUBIN(1:3).
     MOVE    "-"                 TO        PRT-TYUBIN(4:1).
     MOVE    SHI-F122            TO        PRT-TYUBIN(5:4).
     MOVE    SHI-F07             TO        PRT-TJYUSH.
     MOVE    SHI-F09             TO        PRT-TTELNO.
     MOVE    SHI-F10             TO        PRT-TFAXNO.
*    出力特販部
     MOVE     HEN-TOKHAN         TO        PRT-TOKHAN.
 JISHA-EXIT.
     EXIT.
***********************************************************
*            明細情報　　　転送
***********************************************************
 MEISAI-SEC                 SECTION.
*商品名（商品名称Ｍ）の検索
     MOVE    BDY-F06             TO        MEI-F011.
     MOVE    BDY-F07             TO        MEI-F012.
     READ    HMEIMS
       INVALID KEY
             MOVE SPACE          TO        PRT-SNM1(IX)
             MOVE SPACE          TO        PRT-SNM2(IX)
       NOT INVALID KEY
             MOVE MEI-F021       TO        PRT-SNM1(IX)
             MOVE MEI-F022       TO        PRT-SNM2(IX)
     END-READ.
     MOVE    BDY-F06             TO        PRT-SHOC(IX)(2:8)
     MOVE    "("                 TO        PRT-SHOC(IX)(1:1)
     MOVE    ")"                 TO        PRT-SHOC(IX)(10:1)
*単位（条件Ｆ）の検索
     MOVE    "70"                TO        JYO-F01.
     MOVE    BDY-F07(6:2)        TO        JYO-F02.
     READ    HJYOKEN
       INVALID KEY
             MOVE SPACE          TO        PRT-TANI(IX)
       NOT INVALID KEY
             MOVE JYO-F03        TO        PRT-TANI(IX)
     END-READ.
*単位（条件Ｆ）の検索
     MOVE    "72"                TO        JYO-F01.
     MOVE    BDY-F07(8:1)        TO        JYO-F02.
     READ    HJYOKEN
       INVALID KEY
             MOVE SPACE          TO        PRT-TANNM(IX)
       NOT INVALID KEY
             MOVE JYO-F03        TO        PRT-TANNM(IX)
     END-READ.
     MOVE    BDY-F07(1:5)        TO        PRT-TAN1(IX).
*数量
     MOVE    BDY-F09             TO        WK-SURYO.
     MOVE    WK-SURYOA           TO        PRT-SUUA(IX).
     IF  (WK-SURYOB    NOT =    ZERO)
         MOVE     WK-SURYOB      TO        PRT-SUUB(IX)
     END-IF.
*単価出力ありのみ単価、金額を出力する
     IF  DSP-TANOUT  =    1
         GO  TO   MEISAI-010
     END-IF.
*単価
     IF  (BDY-F11    NOT =    3)
         MOVE    BDY-F12             TO        WK-SITAN
         MOVE    WK-SITANA           TO        PRT-STAA(IX)
         IF  (WK-SITANB    NOT =    ZERO)
             MOVE     WK-SITANB      TO        PRT-STAB(IX)
         END-IF
     END-IF.
*金額
     IF  (BDY-F11    NOT =    3)
         COMPUTE PRT-SIRK(IX)  =  BDY-F09  *  BDY-F12
     ELSE
         MOVE    BDY-F12             TO        WK-SITAN
         MOVE    WK-SITANA           TO        PRT-SIRK(IX)
     END-IF.
     ADD     PRT-SIRK(IX)        TO        WK-GOKEI.
*売単価
     IF  (BDY-F01    =    60)
         MOVE    BDY-F14             TO        WK-BATAN
         MOVE    WK-BATANA           TO        PRT-BTAA(IX)
         IF  (WK-BATANB    NOT =    ZERO)
             MOVE     WK-BATANB      TO        PRT-BTAB(IX)
         END-IF
     END-IF.
*
 MEISAI-010.
*
*備考
     MOVE    BDY-F16             TO        PRT-BIKO(IX).
 MEISAI-EXIT.
     EXIT.
***********************************************************
*         取引先名
***********************************************************
 TORIHIK-SEC               SECTION.
*取引先ＭのＲＥＡＤ
     MOVE    HED-F07             TO        TOK-F01.
     READ    HTOKMS
       INVALID KEY
             GO                  TO        TORIHIKI-EXIT
     END-READ.
     MOVE    TOK-F04             TO        PRT-TORNM.
 TORIHIKI-EXIT.
     EXIT.
**********************************************************
*            合計情報
**********************************************************
 GOKEI-SEC                SECTION.
*項目転送
     IF  DSP-TANOUT  =    1
         GO  TO   GOKEI-EXIT
     END-IF.
     MOVE    WK-GOKEI            TO        PRT-GSIRKG.
 GOKEI-EXIT.
     EXIT.
**********************************************************
*            伝票印刷
**********************************************************
 DEN-WT-SEC                SECTION.
*タイトル名印字
     MOVE  NC"注　文　書"        TO        PRT-W001.
*印刷処理
     MOVE    "ALLF"              TO        PRT-GRP.
     MOVE    "FHA00513"          TO        PRT-FORM.
     WRITE   PRT-FHA00513.
*合計クリアー
     MOVE    ZERO                TO        WK-GOKEI.
 DEN-WT-EXIT.
     EXIT.
**********************************************************
*                       Ｅ Ｎ Ｄ                         *
**********************************************************
 END-SEC                   SECTION.
     CLOSE       HACHEDF.
     CLOSE       HACMEIF.
     CLOSE       HTOKMS.
     CLOSE       ZSHIMS.
     CLOSE       HMEIMS.
     CLOSE       ZSOKMS.
     CLOSE       HTENMS.
     CLOSE       HJYOKEN.
     CLOSE       DSPF.
     CLOSE       PRTF.
 END-EXIT.
     EXIT.
******************************************************************
*               発注データ　ＳＴＡＲＴ
******************************************************************
 HED-ST-SEC                SECTION.
     START   HACHEDF     KEY >=  HED-F40  HED-F02
       INVALID KEY
          MOVE   "ERR"           TO        ERR-FLG
       NOT INVALID KEY
           PERFORM HED-RD-SEC
     END-START.
 HED-ST-EXIT.
     EXIT.
******************************************************************
*               発注データ　順読み
******************************************************************
 HED-RD-SEC                SECTION.
     READ    HACHEDF   NEXT
       AT  END
             MOVE      "ERR"     TO        ERR-FLG
             GO                  TO        HED-RD-EXIT
     END-READ.
*伝票_大小チェック*
     IF  (DSP-ENDNO   <    HED-F02)
         MOVE    "ERR"         TO        ERR-FLG
         GO                    TO        HED-RD-EXIT
     END-IF.
*伝票区分
     IF  (HED-F01   =    50)
     OR  (HED-F01   =    60)
     OR  (HED-F01   =    70)
     OR  (HED-F01   =    80)
         CONTINUE
     ELSE
         GO                TO        HED-RD-SEC
     END-IF.
*発注書出力フラグ
     IF  (DSP-KUBUN    =    9)
         IF  (HED-F97  =   1)
             CONTINUE
         ELSE
             GO                TO        HED-RD-SEC
         END-IF
     ELSE
         IF  (HED-F97  =   1)
             GO                TO        HED-RD-SEC
         END-IF
     END-IF.
*取消フラグ
     IF  (HED-F24  NOT =  ZERO)
         GO                TO        HED-RD-SEC
     END-IF.
*相殺区分
*    IF  (HAC-F04  NOT =  ZERO)
*        GO                TO        HED-RD-SEC
*    END-IF.
*枝番
     IF  (HED-F03  NOT =  ZERO)
         GO                TO        HED-RD-SEC
     END-IF.
*発注フラグ
     IF  (HED-F25  NOT =  1)
         GO                TO        HED-RD-SEC
     END-IF.
*伝票区分（７０，８０）
     IF  (HED-F01   =   70   OR  =   80)
     AND (HED-F06   =    32299999)
         GO            TO        HED-RD-SEC
     END-IF.
*承認区分チェック
     IF  (HED-F36  =  SPACE)
         GO                TO        HED-RD-SEC
     END-IF.
*仕入先ＣＤ部門チェック（九州の場合は対象外）
*****IF  HED-F06(1:3)  =  "294"
*********GO                TO        HED-RD-SEC
*****END-IF.
*部門ＣＤチェック
     IF  LNK-M-BUMON  NOT =  HED-F40
         MOVE    "ERR"         TO        ERR-FLG
     END-IF.
 HED-RD-EXIT.
     EXIT.
******************************************************************
*               発注データ（明細）ＳＴＡＲＴ
******************************************************************
 BDY-ST-SEC                SECTION.
     MOVE    HED-F02             TO        BDY-F02.
     MOVE    ZERO                TO        BDY-F03.
*
     START   HACMEIF     KEY >=  BDY-F02   BDY-F03
       INVALID KEY
           MOVE    HIGH-VALUE     TO       WK-BDY-KEY
       NOT INVALID KEY
           PERFORM BDY-RD-SEC
     END-START.
 BDY-ST-EXIT.
     EXIT.
******************************************************************
*               発注データ（明細）順読み
******************************************************************
 BDY-RD-SEC                SECTION.
     READ    HACMEIF   NEXT
       AT  END
             MOVE    HIGH-VALUE  TO        WK-BDY-KEY
             GO                  TO        BDY-RD-EXIT
     END-READ.
*
     MOVE    BDY-F02             TO        WK-BDY-F02.
 BDY-RD-EXIT.
     EXIT.
****************************************************************
*    　      発注書出力フラグの転送 　　
****************************************************************
 HED-RW-SEC             SECTION.
     MOVE    1                   TO   HED-F97.
     REWRITE HED-REC.
 HED-RW-EXIT.
     EXIT.
****************************************************************
* 2          出力区分　　　　入力処理
****************************************************************
 KUBUN-SEC              SECTION.
     MOVE    PFKEY01             TO        DSP-PFKEY.
     PERFORM DSP-WT-SEC.
     MOVE    "KUBUN"             TO        DSP-GRP.
     PERFORM DSP-RD-SEC.
     EVALUATE  DSP-FNC
         WHEN  "F004"       MOVE  1        TO   SHORI-FLG
         WHEN  "F005"       MOVE  "END"    TO   END-FLG
         WHEN  "E000"       PERFORM        CHK-KUBUN-SEC
                            IF    ERR-FLG = SPACE
                                  IF    DSP-KUBUN = 2 OR 9
                                        MOVE    3   TO   SHORI-FLG
                                  ELSE
                                        MOVE    4   TO   SHORI-FLG
                                   IF  DSP-KUBUN   =    3
                                       MOVE  ZERO    TO DSP-STANO
                                       MOVE  9999999 TO DSP-ENDNO
                                       MOVE  6       TO SHORI-FLG
                                   END-IF
                                  END-IF
                            END-IF
         WHEN  OTHER        MOVE  MSG01    TO   DSP-ERRMSG
     END-EVALUATE.
 KUBUN-EXIT.
     EXIT.
****************************************************************
* 2          出力区分　　　　チェック処理                      *
****************************************************************
 CHK-KUBUN-SEC                 SECTION.
     MOVE    SPACE               TO        ERR-FLG.
     IF      DSP-KUBUN     NOT   NUMERIC
         MOVE     ZERO      TO   DSP-KUBUN
     END-IF.
*区分チェック
     IF     (DSP-KUBUN    =    1)
     OR     (DSP-KUBUN    =    2)
     OR     (DSP-KUBUN    =    3)
     OR     (DSP-KUBUN    =    9)
         CONTINUE
     ELSE
         MOVE    "ERR"           TO      ERR-FLG
         MOVE    MSG02           TO      DSP-ERRMSG
     END-IF.
 CHK-KUBUNI-EXIT.
     EXIT.
****************************************************************
* 3          範囲  　　　　　入力処理
****************************************************************
 HANI-SEC              SECTION.
     MOVE    PFKEY02             TO        DSP-PFKEY.
     PERFORM DSP-WT-SEC.
     MOVE    "BODY"          TO      DSP-GRP.
     PERFORM DSP-RD-SEC.
     EVALUATE  DSP-FNC
         WHEN  "F004"       MOVE  1       TO  SHORI-FLG
         WHEN  "F005"       MOVE  "END"   TO  END-FLG
         WHEN  "F006"       MOVE  2       TO  SHORI-FLG
         WHEN  "E000"       PERFORM       CHK-HANI-SEC
                            IF    ERR-FLG = SPACE
                                  MOVE    4    TO  SHORI-FLG
                            END-IF
         WHEN  OTHER        MOVE  MSG01   TO  DSP-ERRMSG
     END-EVALUATE.
 HANI-EXIT.
     EXIT.
****************************************************************
* 3          範囲　　　　　　チェック処理                      *
****************************************************************
 CHK-HANI-SEC                 SECTION.
     MOVE    SPACE               TO        ERR-FLG.
*伝票_チェック
     IF      DSP-STANO     NOT   NUMERIC
        MOVE      ZERO      TO   DSP-STANO
     END-IF.
     IF      DSP-ENDNO     NOT   NUMERIC
     OR      DSP-ENDNO     =     ZERO
         MOVE   9999999     TO      DSP-ENDNO
     END-IF.
     IF      DSP-STANO     >     DSP-ENDNO
         MOVE    "ERR"           TO      ERR-FLG
         MOVE    MSG03           TO      DSP-ERRMSG
         MOVE    "R"         TO   EDIT-OPTION  OF  DSP-STANO
         MOVE    "R"         TO   EDIT-OPTION  OF  DSP-ENDNO
         MOVE    "C"         TO   EDIT-CURSOR  OF  DSP-STANO
     END-IF.
*
     IF      DSP-TANOUT    NOT   NUMERIC
         MOVE    ZERO      TO    DSP-TANOUT
     ELSE
         IF  DSP-TANOUT    NOT =     0  AND  1
             IF  ERR-FLG   =     SPACE
                 MOVE     "ERR"  TO    ERR-FLG
                 MOVE      MSG05 TO    DSP-ERRMSG
             END-IF
             MOVE    "R"   TO   EDIT-OPTION  OF  DSP-TANOUT
             MOVE    "C"   TO   EDIT-CURSOR  OF  DSP-TANOUT
         END-IF
     END-IF.
*
 CHK-HANI-EXIT.
     EXIT.
****************************************************************
* 4          確認  　　　　　入力処理
****************************************************************
 KAKU-SEC              SECTION.
     MOVE    PFKEY02             TO        DSP-PFKEY.
     PERFORM DSP-WT-SEC.
     MOVE    "KAKU"              TO        DSP-GRP.
     PERFORM DSP-RD-SEC.
     EVALUATE  DSP-FNC
         WHEN  "F004"       MOVE  1       TO  SHORI-FLG
         WHEN  "F005"       MOVE  "END"   TO  END-FLG
         WHEN  "F006"       IF    DSP-KUBUN = 1 OR 3
                                  MOVE    2    TO  SHORI-FLG
                            ELSE
                                  MOVE    3    TO  SHORI-FLG
                            END-IF
         WHEN  "E000"       IF    DSP-KUBUN  =  1
                                  MOVE  MSG99  TO  DSP-ERRMSG
                                  PERFORM      DSP-WT-SEC
                                  PERFORM      TEST-SEC
                                  MOVE    1    TO  SHORI-FLG
                            ELSE
                                  PERFORM       CHK-KAKU-SEC
                                  IF    ERR-FLG = SPACE
                                        PERFORM      DSP-WT-SEC
                                        MOVE    5    TO  SHORI-FLG
*#2014/05/20 NAV ST
                                  ELSE
                                        MOVE    2    TO  SHORI-FLG
*発注ヘッダＦをＣＬＯＳＥ／ＯＰＥＮする。
                                        CLOSE              HACHEDF
                                        OPEN    I-O        HACHEDF
*#2014/05/20 NAV ED
                                  END-IF
                            END-IF
         WHEN  OTHER        MOVE  MSG01   TO  DSP-ERRMSG
     END-EVALUATE.
 KAKU-EXIT.
     EXIT.
****************************************************************
* 4          確認　　　　　　チェック処理                      *
****************************************************************
 CHK-KAKU-SEC                 SECTION.
*伝票_チェック
     IF      DSP-STANO     NOT   NUMERIC
        MOVE      ZERO      TO   DSP-STANO
     END-IF.
     IF      DSP-ENDNO     NOT   NUMERIC
     OR      DSP-ENDNO     =     ZERO
         MOVE   9999999     TO   DSP-ENDNO
     END-IF.
*発注データ存在チェック
     MOVE    LNK-M-BUMON         TO        HED-F40.
     MOVE    DSP-STANO           TO        HED-F02.
     PERFORM HED-ST-SEC.
     IF  (ERR-FLG     =    SPACE)
          CONTINUE
       ELSE
          MOVE   MSG04           TO        DSP-ERRMSG
          GO                     TO        CHK-KAKU-EXIT
     END-IF.
*伝票_大小チェック*
     IF  (DSP-ENDNO   <    HED-F02)
          MOVE   "ERR"           TO        ERR-FLG
          MOVE   MSG04           TO        DSP-ERRMSG
       ELSE
          MOVE   MSG99           TO        DSP-ERRMSG
     END-IF.
 CHK-KAKU-EXIT.
     EXIT.
****************************************************************
* 6          範囲  　　　　　入力処理
****************************************************************
 TANK-SEC              SECTION.
     MOVE    PFKEY02         TO        DSP-PFKEY.
     PERFORM DSP-WT-SEC.
     MOVE    "BODY2"         TO        DSP-GRP.
     PERFORM DSP-RD-SEC.
     EVALUATE  DSP-FNC
         WHEN  "F004"       MOVE  1       TO  SHORI-FLG
         WHEN  "F005"       MOVE  "END"   TO  END-FLG
         WHEN  "F006"       MOVE  1       TO  SHORI-FLG
         WHEN  "E000"       PERFORM       TANK-CHK-SEC
                            IF    ERR-FLG = SPACE
                                  MOVE    4    TO  SHORI-FLG
                            END-IF
         WHEN  OTHER        MOVE  MSG01   TO  DSP-ERRMSG
     END-EVALUATE.
 TANK-EXIT.
     EXIT.
****************************************************************
* 6          範囲　　　　　　チェック処理                      *
****************************************************************
 TANK-CHK-SEC                 SECTION.
     MOVE    SPACE               TO        ERR-FLG.
     IF      DSP-TANOUT    NOT   NUMERIC
         MOVE    ZERO      TO    DSP-TANOUT
     ELSE
         IF  DSP-TANOUT    NOT =     0  AND  1
             IF  ERR-FLG   =     SPACE
                 MOVE     "ERR"  TO    ERR-FLG
                 MOVE      MSG05 TO    DSP-ERRMSG
             END-IF
             MOVE    "R"   TO   EDIT-OPTION  OF  DSP-TANOUT
             MOVE    "C"   TO   EDIT-CURSOR  OF  DSP-TANOUT
         END-IF
     END-IF.
*
 TANK-CHK-EXIT.
     EXIT.
***********************************************************
*                    画面初期設定                         *
***********************************************************
 DSP-INIT-SEC               SECTION.
     MOVE    SPACE               TO        DSP-FHA00511.
     MOVE    SPACE               TO        DSP-AREA.
     MOVE    HEN-DATE            TO        DSP-SDATE.
     MOVE    HEN-TIME            TO        DSP-STIME.
     MOVE    HEN-TOKHAN-AREA     TO        DSP-TOKHAN.
     MOVE    "FHA00511"          TO        DSP-FORM.
     PERFORM DSP-WT-SEC.
     MOVE    2                   TO        SHORI-FLG.
 DSP-INIT-EXIT.
     EXIT.
****************************************************
*---画面リード---*
****************************************************
 DSP-RD-SEC                SECTION.
     MOVE    "NE"          TO   DSP-PROC.
     READ                       DSPF.
 DSP-RD-EXIT.
     EXIT.
****************************************************
*---画面ライト---*
****************************************************
 DSP-WT-SEC                SECTION.
     MOVE    "SCREEN"      TO   DSP-GRP.
     MOVE    SPACE         TO   DSP-PROC.
     WRITE                      DSP-FHA00511.
     MOVE    SPACE         TO   DSP-ERRMSG.
     MOVE    SPACE         TO   ERR-FLG.
 DSP-WT-EXIT.
     EXIT.
****************************************************************
*    　      再入力　　　　　処理  　　
****************************************************************
 SAISI-SEC              SECTION.
     CLOSE              PRTF.
     OPEN    OUTPUT     PRTF.
*発注ヘッダＦをＣＬＯＳＥ／ＯＰＥＮする。
     CLOSE              HACHEDF.
     OPEN    I-O        HACHEDF.
*
     MOVE    1             TO  SHORI-FLG.
*
     IF  LNK-M-BUMON  =  "2940"
         MOVE  "END"       TO  END-FLG
     END-IF.
*
 SAISI-EXIT.
     EXIT.
***********************************************************
*            テストプリント
***********************************************************
 TEST-SEC                 SECTION.
     MOVE    SPACE               TO        PRT-FHA00513.
*仕入先情報
     MOVE    ALL "*"             TO        PRT-DENKB.
     MOVE    "("                 TO        PRT-SSIRCD(1:1).
     MOVE    ")"                 TO        PRT-SSIRCD(10:1).
     MOVE    ALL "*"             TO        PRT-SSIRCD(2:8).
     MOVE    ALL NC"＊"          TO        PRT-SYUBIN
                                           PRT-SJYUS1
                                           PRT-SJYUS2
                                           PRT-SSIRNM
                                           PRT-SBUKNM.
*納入先情報
     MOVE    ALL "*"             TO        PRT-NNOUCD(2:8).
     MOVE    "("                 TO        PRT-NNOUCD(1:1).
     MOVE    ")"                 TO        PRT-NNOUCD(10:1).
     MOVE    ALL "*"             TO        PRT-NYUBIN.
     MOVE    ALL NC"＊"          TO        PRT-NJYUS1
                                           PRT-NJYUS2
                                           PRT-NNOUNM.
     MOVE    ALL "*"             TO        PRT-NTELNO.
*自社情報
     MOVE    ALL "9"             TO        PRT-SYSYY
                                           PRT-SYSMM
                                           PRT-SYSDD.
     MOVE    ALL "*"             TO        PRT-TYUBIN
                                           PRT-TTELNO
                                           PRT-TFAXNO.
     MOVE    ALL NC"＊"          TO        PRT-TJYUSH.
*伝票情報
     MOVE    ALL NC"＊"          TO        PRT-TOKHAN.
     MOVE    ALL NC"＊"          TO        PRT-DENKNM.
     MOVE    ALL "9"             TO        PRT-DENNO
                                           PRT-HACDT
                                           PRT-RYONO.
     MOVE    ALL "9"             TO        PRT-NOUDT.
     MOVE    ALL "*"             TO        PRT-SORYKB
                                           PRT-MEMONO.
*明細情報
     PERFORM    VARYING   IX   FROM   1   BY   1
                                          UNTIL  IX  >  6
        MOVE    ALL NC"＊"       TO        PRT-SNM1(IX)
                                           PRT-SNM2(IX)
                                           PRT-TANI(IX)
***** < 93/06/01 START > *****
                                           PRT-TANNM(IX)
***** < 93/06/01  END  > *****
        MOVE    ALL "*"          TO        PRT-SHOC(IX)
                                           PRT-BIKO(IX)
***** < 93/06/01 START > *****
                                           PRT-TAN1(IX)
***** < 93/06/01  END  > *****
        MOVE    ALL "9"          TO        PRT-SUUA(IX)
                                           PRT-SUUB(IX)
                                           PRT-STAA(IX)
                                           PRT-STAB(IX)
                                           PRT-SIRK(IX)
                                           PRT-BTAA(IX)
                                           PRT-BTAB(IX)
     END-PERFORM.
*合計情報
     MOVE    ALL "*"             TO        PRT-TORNM.
     MOVE    ALL "9"             TO        PRT-GSIRKG.
     MOVE    ALL "*"             TO        PRT-TANCD
                                           PRT-TEKCD
                                           PRT-TEKNM1
                                           PRT-TEKNM2.
*印刷処理
     MOVE    "ALLF"              TO        PRT-GRP.
     MOVE    "FHA00513"          TO        PRT-FORM.
     WRITE   PRT-FHA00513.
*ファイル
     CLOSE   PRTF.
     OPEN    OUTPUT    PRTF.
 TEST-EXIT.
     EXIT.

```
