# ZHA0080B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/ZHA0080B.COB`

## ソースコード

```cobol
**********************************************************
*                                                        *
*            サカタのタネ　　 発注書 　
*                                                        *
**********************************************************
 IDENTIFICATION            DIVISION.
 PROGRAM-ID.               ZHA0080B.
 AUTHOR.                   Y.Y.
 DATE-WRITTEN.             93/05/14.
**********************************************************
 ENVIRONMENT               DIVISION.
**********************************************************
 CONFIGURATION             SECTION.
 SOURCE-COMPUTER.
 OBJECT-COMPUTER.
 SPECIAL-NAMES.
     STATION     IS        STA.
***********************************************************
*             INPUT-OUTPUT                                *
***********************************************************
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*発注ファイル
     SELECT      ZHACHDT   ASSIGN    TO        DA-01-VI-ZHACHDT2
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      DYNAMIC
                           RECORD    KEY       HAC-F02   HAC-F03
                                               HAC-F04   HAC-F05
                           FILE      STATUS    HAC-ST.
*発注ファイル（０，８０レコード用）
     SELECT      ZHACHD1   ASSIGN    TO        DA-01-VI-ZHACHDT1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       HA1-F01   HA1-F02
                                               HA1-F03   HA1-F04
                                               HA1-F05
                           FILE      STATUS    HA1-ST.
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
 FD  ZHACHDT.
     COPY        ZHACHDT.
 FD  ZHACHD1.
 01  HA1-REC.
     03  HA1-F01             PIC  9(02).
     03  HA1-F02             PIC  9(07).
     03  HA1-F03             PIC  9(02).
     03  HA1-F04             PIC  9(01).
     03  HA1-F05             PIC  9(02).
     03  HA1-F06             PIC  9(01).
     03  HA1-F07             PIC  X(06).
     03  HA1-F08             PIC  N(18).
     03  HA1-F09             PIC  N(18).
     03  HA1-F10             PIC  N(18).
     03  HA1-F11             PIC  X(02).
     03  HA1-F12             PIC  X(15).
     03  HA1-F13             PIC  X(15).
     03  HA1-F14             PIC  X(15).
     03  HA1-F15             PIC  X(02).
     03  HA1-F16             PIC  X(01).
     03  HA1-F90             PIC  X(04).
     03  HA1-F97             PIC  9(01).
     03  HA1-F98             PIC  9(08).
     03  HA1-F99             PIC  9(08).
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
     COPY        ZHA0080L  OF        XMDLIB
                 JOINING   PRT   AS  PREFIX.
 FD  DSPF.
     COPY        ZHA0080   OF        XMDLIB
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
     03  HAC-ST            PIC X(02).
     03  HA1-ST            PIC X(02).
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
         05  JEF-F062      PIC N(02).
     03  JEF-F07           PIC X(01).
 01  WK-JEF.
     03  WK-JEF01          PIC N(03).
     03  WK-JEF02          PIC N(01).
     03  WK-JEF03          PIC N(02).
****  93/05/21 ****
****　サブルーチン用連絡領域（日本語変換の為）****
 01  EBCC-RTNCD          PIC S9(09)  BINARY.
 01  EBCC-INLT           PIC S9(04)  BINARY  VALUE  5.
 01  EBCC-INDT           PIC  X(05).
 01  EBCC-OTLT           PIC S9(04)  BINARY  VALUE 10.
 01  EBCC-OTDT           PIC  N(05).
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
*フラグ
 01  FLG-AREA.
     03  END-FLG           PIC X(03)   VALUE  SPACE.
     03  SHORI-FLG         PIC 9(01)   VALUE  ZERO.
     03  ERR-FLG           PIC X(03)   VALUE  SPACE.
     03  HAC-FLG           PIC 9(01)   VALUE  ZERO.
*システム日付
 01  WK-SYSYMD             PIC 9(06).
 01  WK-SYSYMDR   REDEFINES WK-SYSYMD.
     03  WK-SYSYY          PIC 9(02).
     03  WK-SYSMM          PIC 9(02).
     03  WK-SYSDD          PIC 9(02).
 01  FILE-ERR.
     03  HAC-ERR           PIC N(10) VALUE
                        NC"発注ファイルエラー".
     03  HA1-ERR           PIC N(10) VALUE
                        NC"発注検索Ｆエラー".
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
     03  MSG99             PIC  N(20)  VALUE
           NC"『リスト出力中です！』".
*ＰＦメッセージ
 01  PF-AREA.
     03  PFKEY01           PIC  N(20)  VALUE
           NC"_取消　_終了".
     03  PFKEY02           PIC  N(20)  VALUE
           NC"_取消　_終了　_項目戻り".
******************************************************************
*             PROCEDURE           DIVISION                       *
******************************************************************
 PROCEDURE                           DIVISION.
 DECLARATIVES.
 HAC-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE ZHACHDT.
     DISPLAY     HAC-ERR   UPON      STA.
     DISPLAY     HAC-ST    UPON      STA.
     STOP        RUN.
 HA1-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE ZHACHD1.
     DISPLAY     HA1-ERR   UPON      STA.
     DISPLAY     HA1-ST    UPON      STA.
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
     PERFORM     INIT-SEC.
     PERFORM     MAIN-SEC  UNTIL     END-FLG  =  "END".
     PERFORM     END-SEC.
     STOP        RUN.
 PROC-EXIT.
     EXIT.
**********************************************************
*                      Ｉ Ｎ Ｉ Ｔ                       *
**********************************************************
 INIT-SEC                  SECTION.
*各ファイルのＯＰＥＮ
     OPEN    I-O       ZHACHDT.
     OPEN    INPUT     ZHACHD1.
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
         WHEN     9          PERFORM   SAISI-SEC
     END-EVALUATE.
 MAIN-EXIT.
     EXIT.
****************************************************************
*             リスト出力                                 *
****************************************************************
 LIST-SEC          SECTION.
*仕入先情報
     PERFORM SHIRE-SEC.
*納入先情報
     PERFORM NOUNYU-SEC.
*伝票情報
     PERFORM DENPYO-SEC.
*自社情報
     PERFORM JISHA-SEC.
*明細情報
     PERFORM    VARYING   IX   FROM   1   BY   1
         UNTIL  WK-DENNO  NOT =  HAC-F02   OR  IX  >  6
            OR  ERR-FLG   NOT =  SPACE
             PERFORM   MEISAI-SEC
             PERFORM   HAC-RD-SEC
     END-PERFORM.
*合計情報
     PERFORM GOKEI-SEC.
*伝票印刷
     PERFORM DEN-WT-SEC.
     IF  (ERR-FLG    =    "ERR")
         MOVE     9                  TO        SHORI-FLG
     END-IF.
 LIST-EXIT.
     EXIT.
***********************************************************
*            仕入先情報　　　転送
***********************************************************
 SHIRE-SEC                 SECTION.
     MOVE    SPACE               TO        PRT-ZHA0080L.
     MOVE    HAC-F02             TO        WK-DENNO.
*項目転送
     MOVE    HAC-F01             TO        PRT-DENKB.
     MOVE    "("                 TO        PRT-SSIRCD(1:1).
     MOVE    ")"                 TO        PRT-SSIRCD(10:1).
     MOVE    HAC-F08             TO        PRT-SSIRCD(2:8).
*発注書出力フラグの転送
     IF  (HAC-F42   =   ZERO)
         PERFORM HAC-RW-SEC
     END-IF.
*仕入先ＭのＲＥＡＤ
     MOVE    HAC-F08             TO        SHI-F01.
     READ    ZSHIMS
       INVALID KEY
             GO                  TO        SHIRE-EXIT
     END-READ.
*郵便番号（ＪＥＦ変換）
     MOVE    SPACE               TO        WK-JEF.
     IF   (HAC-F06    NOT =   SPACE)
*05/21**  CALL    "CVTEBJEF"   USING  SHI-F06   SHI-F07
*05/21**                              JEF-F06   JEF-F07
*05/21**  MOVE    JEF-F061            TO        WK-JEF01
          MOVE    SHI-F06             TO        EBCC-INDT
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
          MOVE    EBCC-OTDT              TO   JEF-F06
*
          MOVE    JEF-F061       TO        WK-JEF01
          IF   (JEF-F062   NOT =   SPACE)
               MOVE    NC"－"         TO        WK-JEF02
               MOVE    JEF-F062       TO        WK-JEF03
          END-IF
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
*発注データの検索（０レコード）
     MOVE    HAC-F01             TO        HA1-F01.
     MOVE    HAC-F02             TO        HA1-F02.
     MOVE    HAC-F03             TO        HA1-F03.
     MOVE    HAC-F04             TO        HA1-F04.
     MOVE    ZERO                TO        HA1-F05.
     PERFORM HA1-RD-SEC.
*項目転送の検索
     EVALUATE  HAC-F01
        WHEN   50
             IF   (HAC-F19    =    0)
                   PERFORM  NOU-TEN1-SEC
             ELSE
                   PERFORM  NOU-TEN5-SEC
             END-IF
        WHEN   60
             IF   (HAC-FLG    =    0)
                  PERFORM  NOU-TEN2-SEC
             ELSE
                  IF   (HAC-F19    =    0)
                       PERFORM  NOU-TEN4-SEC
                  ELSE
                       PERFORM  NOU-TEN5-SEC
                  END-IF
             END-IF
        WHEN   70
        WHEN   80
             IF   (HAC-FLG    =    0)
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
     MOVE    HAC-F31             TO        PRT-NNOUCD(2:2).
     MOVE    "("                 TO        PRT-NNOUCD(1:1).
     MOVE    ")"                 TO        PRT-NNOUCD(10:1).
*発注書出力フラグの転送
     IF  (HAC-F42   =   ZERO)
         PERFORM HAC-RW-SEC
     END-IF.
*倉庫ＭのＲＥＡＤ
     MOVE    HAC-F31             TO        SOK-F01.
     READ    ZSOKMS
       INVALID KEY
             GO                  TO        NOU-TEN1-EXIT
     END-READ.
     MOVE    SOK-F04(1:3)        TO        PRT-NYUBIN(1:3).
     IF   (SOK-F04(4:2)   NOT =   SPACE)
          MOVE    "-"            TO        PRT-NYUBIN(4:1)
          MOVE    SOK-F04(4:2)   TO        PRT-NYUBIN(5:2)
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
     MOVE    HAC-F10             TO        PRT-NNOUCD(2:8).
     MOVE    "("                 TO        PRT-NNOUCD(1:1).
     MOVE    ")"                 TO        PRT-NNOUCD(10:1).
     MOVE    HA1-F07             TO        PRT-NYUBIN.
     MOVE    HA1-F08             TO        PRT-NJYUS1.
     MOVE    HA1-F09             TO        PRT-NJYUS2.
     MOVE    HA1-F10             TO        PRT-NNOUNM.
     MOVE    HA1-F14             TO        PRT-NTELNO.
 NOU-TEN2-EXIT.
     EXIT.
***********************************************************
*            伝票区分    ＝ ７０，８０
*            行_    NOT ＝ ０
***********************************************************
 NOU-TEN3-SEC              SECTION.
*項目転送
     MOVE    HAC-F10             TO        PRT-NNOUCD(2:8).
     MOVE    "("                 TO        PRT-NNOUCD(1:1).
     MOVE    ")"                 TO        PRT-NNOUCD(10:1).
*発注書出力フラグの転送
     IF  (HAC-F42   =   ZERO)
         PERFORM HAC-RW-SEC
     END-IF.
*倉庫ＭのＲＥＡＤ
     MOVE    HAC-F32             TO        SOK-F01.
     READ    ZSOKMS
       INVALID KEY
             GO                  TO        NOU-TEN3-EXIT
     END-READ.
     MOVE    SOK-F04(1:3)        TO        PRT-NYUBIN(1:3).
     IF   (SOK-F04(4:2)   NOT =   SPACE)
          MOVE    "-"            TO        PRT-NYUBIN(4:1)
          MOVE    SOK-F04(4:2)   TO        PRT-NYUBIN(5:2)
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
     MOVE    HAC-F10             TO        PRT-NNOUCD(2:8).
     MOVE    "("                 TO        PRT-NNOUCD(1:1).
     MOVE    ")"                 TO        PRT-NNOUCD(10:1).
*発注書出力フラグの転送
     IF  (HAC-F42   =   ZERO)
         PERFORM HAC-RW-SEC
     END-IF.
*取引先ＭのＲＥＡＤ
     MOVE    HAC-F10             TO        TOK-F01.
     READ    HTOKMS
       INVALID KEY
             GO                  TO        NOU-TEN4-EXIT
     END-READ.
     MOVE    TOK-F05             TO        PRT-NYUBIN.
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
     MOVE    HAC-F10             TO        PRT-NNOUCD(2:8).
     MOVE    "("                 TO        PRT-NNOUCD(1:1).
     MOVE    ")"                 TO        PRT-NNOUCD(10:1).
*発注書出力フラグの転送
     IF  (HAC-F42   =   ZERO)
         PERFORM HAC-RW-SEC
     END-IF.
*店舗ＭのＲＥＡＤ
     MOVE    HAC-F10             TO        TEN-F52.
     MOVE    HAC-F19             TO        TEN-F011.
     READ    HTENMS
       INVALID KEY
             GO                  TO        NOU-TEN5-EXIT
     END-READ.
     MOVE    TEN-F05             TO        PRT-NYUBIN.
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
     MOVE    HAC-F01             TO        JYO-F02.
     READ    HJYOKEN
       INVALID KEY
             MOVE SPACE          TO        PRT-DENKNM
       NOT INVALID KEY
             MOVE JYO-F03        TO        PRT-DENKNM
     END-READ.
     MOVE    HAC-F02             TO        PRT-DENNO.
     MOVE    HAC-F12             TO        PRT-HACDT.
     IF  (HAC-F07    NOT =    ZERO)
         MOVE    HAC-F07         TO        PRT-RYONO
     END-IF.
     MOVE    HAC-F13             TO        PRT-NOUDT.
     IF  (HAC-F16    NOT =    ZERO)
         MOVE    HAC-F16         TO        PRT-SORYKB
     END-IF.
     MOVE    HAC-F18             TO        PRT-MEMONO.
*＊合計情報の転送＊＊
*担当者コード
     MOVE    HAC-F09             TO        PRT-TANCD.
*発注データの検索（8０レコード）
     MOVE    HAC-F01             TO        HA1-F01.
     MOVE    HAC-F02             TO        HA1-F02.
     MOVE    HAC-F03             TO        HA1-F03.
     MOVE    HAC-F04             TO        HA1-F04.
     MOVE    80                  TO        HA1-F05.
     PERFORM HA1-RD-SEC.
     IF  (HAC-FLG    =    0)
         MOVE    HA1-F11             TO        PRT-TEKCD
         MOVE    HA1-F12             TO        PRT-TEKNM1
         MOVE    HA1-F13             TO        PRT-TEKNM2
     END-IF.
*発注書出力フラグの転送
     IF  (HAC-F42   =   ZERO)
         PERFORM HAC-RW-SEC
     END-IF.
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
     MOVE    "29099999"          TO        SHI-F01.
     READ    ZSHIMS
       INVALID KEY
             GO                  TO        JISHA-EXIT
     END-READ.
     MOVE    SHI-F06(1:3)        TO        PRT-TYUBIN(1:3).
     IF   (SOK-F04(4:2)   NOT =   SPACE)
          MOVE    "-"            TO        PRT-TYUBIN(4:1)
          MOVE    SHI-F06(4:2)   TO        PRT-TYUBIN(5:2)
     END-IF.
     MOVE    SHI-F07             TO        PRT-TJYUSH.
     MOVE    SHI-F09             TO        PRT-TTELNO.
     MOVE    SHI-F10             TO        PRT-TFAXNO.
 JISHA-EXIT.
     EXIT.
***********************************************************
*            明細情報　　　転送
***********************************************************
 MEISAI-SEC                 SECTION.
*商品名（商品名称Ｍ）の検索
     MOVE    HAC-F20             TO        MEI-F011.
     MOVE    HAC-F21             TO        MEI-F012.
     READ    HMEIMS
       INVALID KEY
             MOVE SPACE          TO        PRT-SNM1(IX)
             MOVE SPACE          TO        PRT-SNM2(IX)
       NOT INVALID KEY
             MOVE MEI-F021       TO        PRT-SNM1(IX)
             MOVE MEI-F022       TO        PRT-SNM2(IX)
     END-READ.
     MOVE    HAC-F20             TO        PRT-SHOC(IX)(2:8)
     MOVE    "("                 TO        PRT-SHOC(IX)(1:1)
     MOVE    ")"                 TO        PRT-SHOC(IX)(10:1)
*単位（条件Ｆ）の検索
     MOVE    "70"                TO        JYO-F01.
     MOVE    HAC-F21(6:2)        TO        JYO-F02.
     READ    HJYOKEN
       INVALID KEY
             MOVE SPACE          TO        PRT-TANI(IX)
       NOT INVALID KEY
             MOVE JYO-F03        TO        PRT-TANI(IX)
     END-READ.
***** < 93/06/01 START > *****
*単位（条件Ｆ）の検索
     MOVE    "72"                TO        JYO-F01.
     MOVE    HAC-F21(8:1)        TO        JYO-F02.
     READ    HJYOKEN
       INVALID KEY
             MOVE SPACE          TO        PRT-TANNM(IX)
       NOT INVALID KEY
             MOVE JYO-F03        TO        PRT-TANNM(IX)
     END-READ.
     MOVE    HAC-F21(1:5)        TO        PRT-TAN1(IX).
***** < 93/06/01  END  > *****
*数量
     MOVE    HAC-F22             TO        WK-SURYO.
     MOVE    WK-SURYOA           TO        PRT-SUUA(IX).
     IF  (WK-SURYOB    NOT =    ZERO)
         MOVE     WK-SURYOB      TO        PRT-SUUB(IX)
     END-IF.
*単価
     IF  (HAC-F24    NOT =    3)
         MOVE    HAC-F25             TO        WK-SITAN
         MOVE    WK-SITANA           TO        PRT-STAA(IX)
         IF  (WK-SITANB    NOT =    ZERO)
             MOVE     WK-SITANB      TO        PRT-STAB(IX)
         END-IF
     END-IF.
*金額
     IF  (HAC-F24    NOT =    3)
         COMPUTE PRT-SIRK(IX)  =  HAC-F22  *  HAC-F25
     ELSE
         MOVE    HAC-F25             TO        WK-SITAN
         MOVE    WK-SITANA           TO        PRT-SIRK(IX)
     END-IF.
     ADD     PRT-SIRK(IX)        TO        WK-GOKEI.
*売単価
     IF  (HAC-F01    =    60)
         MOVE    HAC-F27             TO        WK-BATAN
         MOVE    WK-BATANA           TO        PRT-BTAA(IX)
         IF  (WK-BATANB    NOT =    ZERO)
             MOVE     WK-BATANB      TO        PRT-BTAB(IX)
         END-IF
     END-IF.
*備考
     MOVE    HAC-F29             TO        PRT-BIKO(IX).
*発注書出力フラグの転送
     IF  (HAC-F42   =   ZERO)
         PERFORM HAC-RW-SEC
     END-IF.
 MEISAI-EXIT.
     EXIT.
**********************************************************
*            合計情報
**********************************************************
 GOKEI-SEC                SECTION.
*項目転送
     MOVE    WK-GOKEI            TO        PRT-GSIRKG.
 GOKEI-EXIT.
     EXIT.
**********************************************************
*            伝票印刷
**********************************************************
 DEN-WT-SEC                SECTION.
*印刷処理
     MOVE    "ALLF"              TO        PRT-GRP.
     MOVE    "ZHA0080L"          TO        PRT-FORM.
     WRITE   PRT-ZHA0080L.
*合計クリアー
     MOVE    ZERO                TO        WK-GOKEI.
 DEN-WT-EXIT.
     EXIT.
**********************************************************
*                       Ｅ Ｎ Ｄ                         *
**********************************************************
 END-SEC                   SECTION.
     CLOSE       ZHACHDT.
     CLOSE       ZHACHD1.
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
 HAC-ST-SEC                SECTION.
     START   ZHACHDT     KEY >=  HAC-F02   HAC-F03
                                 HAC-F04   HAC-F05
       INVALID KEY
          MOVE   "ERR"           TO        ERR-FLG
       NOT INVALID KEY
           PERFORM HAC-RD-SEC
     END-START.
 HAC-ST-EXIT.
     EXIT.
******************************************************************
*               発注データ　順読み
******************************************************************
 HAC-RD-SEC                SECTION.
     READ    ZHACHDT   NEXT
       AT  END
             MOVE      "ERR"     TO        ERR-FLG
             GO                  TO        HAC-RD-EXIT
     END-READ.
*伝票_大小チェック*
     IF  (DSP-ENDNO   <    HAC-F02)
         PERFORM HAC-RW-SEC
         MOVE    "ERR"         TO        ERR-FLG
         GO                    TO        HAC-RD-EXIT
     END-IF.
*伝票区分
     IF  (HAC-F01   =    50)
     OR  (HAC-F01   =    60)
     OR  (HAC-F01   =    70)
     OR  (HAC-F01   =    80)
         CONTINUE
     ELSE
         GO                TO        HAC-RD-SEC
     END-IF.
*行_　　　
     IF  (HAC-F05   >=   1)
     AND (HAC-F05   <=   6)
         CONTINUE
     ELSE
         GO                TO        HAC-RD-SEC
     END-IF.
*発注書出力フラグ
     IF  (DSP-KUBUN    =    9)
         IF  (HAC-F42  =   1)
             CONTINUE
         ELSE
             GO                TO        HAC-RD-SEC
         END-IF
     ELSE
         IF  (HAC-F42  =   1)
             GO                TO        HAC-RD-SEC
         END-IF
     END-IF.
*取消フラグ
     IF  (HAC-F37  NOT =  ZERO)
         GO                TO        HAC-RD-SEC
     END-IF.
*相殺区分
     IF  (HAC-F04  NOT =  ZERO)
         GO                TO        HAC-RD-SEC
     END-IF.
*枝番
     IF  (HAC-F03  NOT =  ZERO)
         GO                TO        HAC-RD-SEC
     END-IF.
*発注フラグ
     IF  (HAC-F36  NOT =  1)
         GO                TO        HAC-RD-SEC
     END-IF.
*伝票区分（７０，８０）
     IF  (HAC-F01   =   70   OR  =   80)
     AND (HAC-F08   =    29099999)
         GO            TO        HAC-RD-SEC
     END-IF.
 HAC-RD-EXIT.
     EXIT.
******************************************************************
*               発注データ　検索
******************************************************************
 HA1-RD-SEC                SECTION.
     MOVE    ZERO                TO   HAC-FLG.
     READ    ZHACHD1
       INVALID  KEY
             MOVE      1         TO        HAC-FLG
             GO                  TO        HA1-RD-EXIT
     END-READ.
*取消フラグ
     IF  (HA1-F97  NOT =  ZERO)
         MOVE      1             TO        HAC-FLG
     END-IF.
 HA1-RD-EXIT.
     EXIT.
****************************************************************
*    　      発注書出力フラグの転送 　　
****************************************************************
 HAC-RW-SEC             SECTION.
     MOVE    1                   TO   HAC-F42.
     REWRITE HAC-REC.
 HAC-RW-EXIT.
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
     END-IF.
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
     MOVE    DSP-STANO           TO        HAC-F02.
     MOVE    ZERO                TO        HAC-F03.
     MOVE    ZERO                TO        HAC-F04.
     MOVE    ZERO                TO        HAC-F05.
     PERFORM HAC-ST-SEC.
     IF  (ERR-FLG     =    SPACE)
          CONTINUE
       ELSE
          MOVE   MSG04           TO        DSP-ERRMSG
          GO                     TO        CHK-KAKU-EXIT
     END-IF.
*伝票_大小チェック*
     IF  (DSP-ENDNO   <    HAC-F02)
          MOVE   "ERR"           TO        ERR-FLG
          MOVE   MSG04           TO        DSP-ERRMSG
       ELSE
          MOVE   MSG99           TO        DSP-ERRMSG
     END-IF.
 CHK-KAKU-EXIT.
     EXIT.
***********************************************************
*                    画面初期設定                         *
***********************************************************
 DSP-INIT-SEC               SECTION.
     MOVE    SPACE               TO        DSP-ZHA0080.
     MOVE    SPACE               TO        DSP-AREA.
     MOVE    "ZHA0080"           TO        DSP-FORM.
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
     WRITE                      DSP-ZHA0080.
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
     MOVE    1             TO  SHORI-FLG.
 SAISI-EXIT.
     EXIT.
***********************************************************
*            テストプリント
***********************************************************
 TEST-SEC                 SECTION.
     MOVE    SPACE               TO        PRT-ZHA0080L.
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
     MOVE    ALL "9"             TO        PRT-GSIRKG
                                           PRT-TANCD.
     MOVE    ALL "*"             TO        PRT-TEKCD
                                           PRT-TEKNM1
                                           PRT-TEKNM2.
*印刷処理
     MOVE    "ALLF"              TO        PRT-GRP.
     MOVE    "ZHA0080L"          TO        PRT-FORM.
     WRITE   PRT-ZHA0080L.
*ファイル
     CLOSE   PRTF.
     OPEN    OUTPUT    PRTF.
 TEST-EXIT.
     EXIT.

```
