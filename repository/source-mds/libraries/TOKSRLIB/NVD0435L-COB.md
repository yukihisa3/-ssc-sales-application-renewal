# NVD0435L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/NVD0435L.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　Ｄ３６５連携　　　　　　　　　　　*
*    モジュール名　　　　：　出荷指示書発行指示　　　　　　　　*
*    作成日／作成者　　　：　2020/02/29   ASS.II               *
*    処理内容　　　　　　：　入出庫ファイルより範囲指定された　*
*    　　　　　　　　　　　　条件で出荷指示書を発行する。　　　*
****************************************************************
 IDENTIFICATION            DIVISION.
 PROGRAM-ID.               NVD0435L.
 AUTHOR.                   ASS.II.
 DATE-WRITTEN.             20/02/29.
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
****<<  入出庫ファイル　　  >>******************************
     SELECT   DNSFILF   ASSIGN    TO      DA-VI-DNSFILL4
                        FILE STATUS          IS  DNS-ST
                        ORGANIZATION         IS  INDEXED
                        ACCESS  MODE         IS  SEQUENTIAL
                        RECORD  KEY          IS  DNS-F05  DNS-F12
                                                 DNS-F01  DNS-F02.
****<< 商品名称マスタ       >>******************************
     SELECT   HMEIMS    ASSIGN  TO   DA-01-VI-MEIMS1
                        ORGANIZATION         IS   INDEXED
                        ACCESS  MODE         IS   RANDOM
                        RECORD  KEY          IS   MEI-F011
                                                  MEI-F012
                        FILE    STATUS       IS   MEI-ST.
****<< サブ商品名称マスタ >>********************************
     SELECT   SUBMEIF   ASSIGN    TO        DA-01-VI-SUBMEIL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   SUB-F011
                                                 SUB-F0121
                                                 SUB-F0122
                                                 SUB-F0123
                        FILE      STATUS    IS   SUB-ST.
****<< 倉庫マスタ           >>******************************
     SELECT   ZSOKMS    ASSIGN  TO   DA-01-VI-ZSOKMS1
                        ORGANIZATION         IS   INDEXED
                        ACCESS  MODE         IS   RANDOM
                        RECORD  KEY          IS   SOK-F01
                        FILE    STATUS       IS   SOK-ST.
****<< 担当者マスタ         >>******************************
     SELECT   HTANMS    ASSIGN  TO   DA-01-VI-TANMS1
                        ORGANIZATION         IS   INDEXED
                        ACCESS  MODE         IS   RANDOM
                        RECORD  KEY          IS   TAN-F01
                                                  TAN-F02
                        FILE    STATUS       IS   TAN-ST.
*%* プリンター *%*
     SELECT     PRTF       ASSIGN    TO        GS-PRTF
                           DESTINATION        "PRT"
                           FORMAT              PRT-FORM
                           GROUP               PRT-GRP
                           PROCESSING          PRT-PROC
                           FILE      STATUS    PRT-ST.
******************************************************************
*             DATA                DIVISION                       *
******************************************************************
 DATA                      DIVISION.
 FILE                      SECTION.
****<< 入出庫ファイル >>*********************************
 FD  DNSFILF.
     COPY     DNSFILF   OF        XFDLIB
              JOINING   DNS       PREFIX.
****<< 商品名称マスタ       >>******************************
 FD    HMEIMS
        LABEL     RECORD    IS    STANDARD.
        COPY      HMEIMS    OF    XFDLIB
                  JOINING   MEI   PREFIX.
****<< サブ商品名称マスタ >>*******************************
 FD  SUBMEIF.
     COPY     SUBMEIF   OF        XFDLIB
              JOINING   SUB       PREFIX.
****<< 倉庫マスタ           >>******************************
 FD    ZSOKMS
        LABEL     RECORD    IS    STANDARD.
        COPY      ZSOKMS    OF    XFDLIB
                  JOINING   SOK   PREFIX.
****<<担当者マスタ         >>******************************
 FD    HTANMS
        LABEL     RECORD    IS    STANDARD.
        COPY      HTANMS    OF    XFDLIB
                  JOINING   TAN   PREFIX.
 FD  PRTF.
     COPY        FVD04351  OF        XMDLIB
                 JOINING   PRT   AS  PREFIX.
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
     03  DNS-ST            PIC X(02).
     03  MEI-ST            PIC X(02).
     03  SOK-ST            PIC X(02).
     03  TAN-ST            PIC X(02).
     03  SUB-ST            PIC X(02).
     03  PRT-ST            PIC X(02).
     03  DSP-ST            PIC X(02).
 01  CNT-AREA.
     03  READ-CNT          PIC  9(07)  VALUE  ZERO.
     03  PAGE-CNT          PIC  9(07)  VALUE  ZERO.
     03  LINE-CNT          PIC  9(02)  VALUE  ZERO.
     03  MAX-LINE          PIC  9(02)  VALUE  ZERO.
     03  IX1               PIC  9(02)  VALUE  ZERO.
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
     03  HMEIMS-INV-FLG    PIC X(03)   VALUE  SPACE.
     03  SUBMEIF-INV-FLG   PIC X(03)   VALUE  SPACE.
     03  ZSOKMS-INV-FLG    PIC X(03)   VALUE  SPACE.
     03  HTANMS-INV-FLG    PIC X(03)   VALUE  SPACE.
*****03  HAC-FLG           PIC 9(01)   VALUE  ZERO.
*システム日付
 01  WK-SYSYMD             PIC 9(06).
 01  WK-SYSYMDR   REDEFINES WK-SYSYMD.
     03  WK-SYSYY          PIC 9(02).
     03  WK-SYSMM          PIC 9(02).
     03  WK-SYSDD          PIC 9(02).
 01  FILE-ERR.
     03  DNS-ERR           PIC N(15) VALUE
                        NC"新入出庫ファイルエラー".
     03  MEI-ERR           PIC N(15) VALUE
                        NC"商品名称マスタエラー".
     03  SOK-ERR           PIC N(15) VALUE
                        NC"倉庫マスタエラー".
     03  SUB-ERR           PIC N(15) VALUE
                        NC"サブ商品名称マスタエラー".
     03  TAN-ERR           PIC N(15) VALUE
                        NC"担当者マスタエラー".
     03  PRT-ERR           PIC N(15) VALUE
                        NC"プリンタＦエラー".
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD8         PIC 9(08).
 LINKAGE                   SECTION.
 01  LINK-IN-BUMON         PIC X(04).
 01  LINK-IN-JSOK          PIC X(02).
 01  LINK-IN-DSOK          PIC X(02).
 01  LINK-IN-SSOK          PIC X(02).
 01  LINK-IN-SAGYOST       PIC 9(08).
 01  LINK-IN-SAGYOEN       PIC 9(08).
 01  LINK-IN-DENNOST       PIC 9(07).
 01  LINK-IN-DENNOEN       PIC 9(07).
 01  LINK-IN-HAKKBN        PIC X(01).
******************************************************************
*             PROCEDURE           DIVISION                       *
******************************************************************
 PROCEDURE                 DIVISION  USING
                                            LINK-IN-BUMON
                                             LINK-IN-JSOK
                                             LINK-IN-DSOK
                                             LINK-IN-SSOK
                                             LINK-IN-SAGYOST
                                             LINK-IN-SAGYOEN
                                             LINK-IN-DENNOST
                                             LINK-IN-DENNOEN
                                             LINK-IN-HAKKBN.
 DECLARATIVES.
 DNS-ERR-SEC               SECTION.
     USE         AFTER     EXCEPTION PROCEDURE DNSFILF.
     DISPLAY     DNS-ERR   UPON      CONS.
     DISPLAY     DNS-ST    UPON      CONS.
     STOP        RUN.
 MEI-ERR-SEC               SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HMEIMS.
     DISPLAY     MEI-ERR   UPON      CONS.
     DISPLAY     MEI-ST    UPON      CONS.
     STOP        RUN.
 SOK-ERR-SEC               SECTION.
     USE         AFTER     EXCEPTION PROCEDURE ZSOKMS.
     DISPLAY     SOK-ERR   UPON      CONS.
     DISPLAY     SOK-ST    UPON      CONS.
     STOP        RUN.
 TAN-ERR-SEC               SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HTANMS.
     DISPLAY     TAN-ERR   UPON      CONS.
     DISPLAY     TAN-ST    UPON      CONS.
     STOP        RUN.
 SUB-ERR-SEC               SECTION.
     USE         AFTER     EXCEPTION PROCEDURE SUBMEIF.
     DISPLAY     SUB-ERR   UPON      CONS.
     DISPLAY     SUB-ST    UPON      CONS.
     STOP        RUN.
 PRT-ERR-SEC               SECTION.
     USE         AFTER     EXCEPTION PROCEDURE PRTF.
     DISPLAY     PRT-ERR   UPON      CONS.
     DISPLAY     PRT-ST    UPON      CONS.
     STOP        RUN.
 END DECLARATIVES.
***********************************************************
*                     ＭＡＩＮ処理                        *
***********************************************************
 PROC-SEC                  SECTION.
     DISPLAY  "**  NVD0435L   START  **"   UPON  CONS.
**************************
*    MOVE  "01"       TO LINK-IN-BUMON   .
*    MOVE  "01"       TO  LINK-IN-JSOK    .
*    MOVE  "01"       TO  LINK-IN-DSOK    .
*    MOVE  "03"       TO  LINK-IN-SSOK    .
*    MOVE  "20200101" TO  LINK-IN-SAGYOST .
*    MOVE  "20200330" TO  LINK-IN-SAGYOEN .
*    MOVE  "0000000"  TO  LINK-IN-DENNOST .
*    MOVE  "9999999"  TO  LINK-IN-DENNOEN .
*    MOVE  " "        TO  LINK-IN-HAKKBN  .

     DISPLAY "部門        ="  LINK-IN-BUMON   UPON CONS.
     DISPLAY "実行倉庫CD  ="  LINK-IN-JSOK    UPON CONS.
     DISPLAY "代表倉庫CD  ="  LINK-IN-DSOK    UPON CONS.
     DISPLAY "出荷倉庫CD  ="  LINK-IN-SSOK    UPON CONS.
     DISPLAY "作業開始日  ="  LINK-IN-SAGYOST UPON CONS.
     DISPLAY "作業終了日  ="  LINK-IN-SAGYOEN UPON CONS.
     DISPLAY "伝票番号開始="  LINK-IN-DENNOST UPON CONS.
     DISPLAY "伝票番号終了="  LINK-IN-DENNOEN UPON CONS.
     DISPLAY "発行済区分  ="  LINK-IN-HAKKBN  UPON CONS.
**************************
*
     PERFORM     INIT-SEC.
     PERFORM     MAIN-SEC  UNTIL     END-FLG  =  "END".
     PERFORM     END-SEC.
*
     DISPLAY  "**  NVD0435L    END   **"   UPON  CONS.
     STOP        RUN.
 PROC-EXIT.
     EXIT.
**********************************************************
*                      Ｉ Ｎ Ｉ Ｔ                       *
**********************************************************
 INIT-SEC                  SECTION.
*各ファイルのＯＰＥＮ
     OPEN    I-O       DNSFILF.
     OPEN    INPUT     HMEIMS.
     OPEN    INPUT     ZSOKMS.
     OPEN    INPUT     HTANMS.
     OPEN    INPUT     SUBMEIF.
     OPEN    OUTPUT    PRTF.
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
     MOVE     ZERO                TO   LINK-OUT-YMD8.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD8.
     MOVE      LINK-OUT-YMD8      TO   SYS-DATE.
*画面表示日付編集
     MOVE      SYS-DATE(1:4)      TO   HEN-DATE-YYYY.
     MOVE      SYS-DATE(5:2)      TO   HEN-DATE-MM.
     MOVE      SYS-DATE(7:2)      TO   HEN-DATE-DD.
*画面表示時刻編集
     MOVE      SYS-TIME(1:2)      TO   HEN-TIME-HH.
     MOVE      SYS-TIME(3:2)      TO   HEN-TIME-MM.
     MOVE      SYS-TIME(5:2)      TO   HEN-TIME-SS.
*入出庫ファイル読み込み
*？？？？スタートキーセット
     PERFORM   DNSFILF-START-SEC
     IF        END-FLG        NOT =   "END"
        PERFORM     DNSFILF-READ-SEC
********伝票番号
        MOVE   DNS-F01            TO  WK-DENNO
        MOVE   99                 TO  LINE-CNT
        MOVE   ZERO               TO  PAGE-CNT
     END-IF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*    MAIN-SEC                                                  *
****************************************************************
 MAIN-SEC          SECTION.
*
*****伝票番号
     IF       DNS-F01        NOT =   WK-DENNO  OR
              LINE-CNT          >=   6
        IF    PAGE-CNT       NOT =   ZERO
           PERFORM LIST-OUT-SEC
        END-IF
        PERFORM  LIST-EDIT-SEC
     END-IF.
     ADD      1                  TO  LINE-CNT.
     PERFORM  MEISAI-EDIT-SEC.
     PERFORM  DNSFILF-READ-SEC.
 MAIN-EXIT.
     EXIT.
****************************************************************
*             リスト見出し情報編集
****************************************************************
 LIST-EDIT-SEC      SECTION.
*
     ADD      1                  TO  PAGE-CNT.
     MOVE     SPACE              TO  PRT-FVD04351.
     INITIALIZE                      PRT-FVD04351.

*
     MOVE     "NVD0435L"         TO  PRT-PGMID.
     MOVE     HEN-DATE           TO  PRT-HDATE.
     MOVE     HEN-TIME           TO  PRT-HTIME.
     MOVE     PAGE-CNT           TO  PRT-PAGE.
*出荷倉庫情報編集
*****出庫場所
     MOVE     DNS-F05            TO  PRT-SSOUKO.
     MOVE     DNS-F05            TO  SOK-F01.
     PERFORM  ZSOKMS-READ-SEC.
     IF       ZSOKMS-INV-FLG     =   SPACE
         MOVE SOK-F02            TO  PRT-SSOKNM
     END-IF.
*****作業日
     MOVE     DNS-F12            TO  PRT-SGDAT.
*****伝票番号
     MOVE     DNS-F01            TO  PRT-DENNO.
     MOVE     ZERO               TO  PRT-YOTNO.
*入荷倉庫情報編集
*****入庫場所
     MOVE     DNS-F08            TO  PRT-NSOUKO.
     MOVE     DNS-F08            TO  SOK-F01.

     PERFORM  ZSOKMS-READ-SEC.
     IF       ZSOKMS-INV-FLG     =   SPACE
         MOVE SOK-F02            TO  PRT-NSOKNM
         MOVE SOK-F04            TO  PRT-YUUNO
         MOVE SOK-F05            TO  PRT-ADDR1
         MOVE SOK-F06            TO  PRT-ADDR2
         MOVE SOK-F07            TO  PRT-TELNO
         MOVE SOK-F08            TO  PRT-FAXNO
     END-IF.
*
*テール部編集
*
     MOVE     ZERO               TO  LINE-CNT.
     MOVE     DNS-F01            TO  WK-DENNO.
 LIST-EDIT-EXIT.
     EXIT.
***********************************************************
*            明細情報編集
***********************************************************
 MEISAI-EDIT-SEC                 SECTION.
*
     MOVE    LINE-CNT            TO        IX1.
*****商品ＣＤ
     MOVE    DNS-F03             TO        PRT-SYOCD(IX1).
*商品名（商品名称Ｍ）の検索
     MOVE    DNS-F03             TO        MEI-F011.
*****品単ＣＤ
     MOVE    DNS-F04             TO        MEI-F012.
     PERFORM HMEIMS-READ-SEC.
     IF      HMEIMS-INV-FLG      =         SPACE
             MOVE MEI-F021       TO        PRT-SYONM1(IX1)
             MOVE MEI-F022       TO        PRT-SYONM2(IX1)
     END-IF.
*
*****商品ＣＤ
     MOVE    DNS-F03             TO        SUB-F011.
*****品単ＣＤ
     MOVE    DNS-F04(1:5)        TO        SUB-F0121.
     MOVE    DNS-F04(6:2)        TO        SUB-F0122.
     MOVE    DNS-F04(8:1)        TO        SUB-F0123.
     PERFORM SUBMEIF-READ-SEC.
     IF      SUBMEIF-INV-FLG     =         SPACE
             MOVE SUB-F06        TO        PRT-JANCD(IX1)
             MOVE SUB-F021       TO        PRT-SYONM1(IX1)
             MOVE SUB-F022       TO        PRT-SYONM2(IX1)
     END-IF.
*
*****出庫_番
     MOVE    DNS-F07             TO        PRT-STANNO(IX1).
*****入庫_番
     MOVE    DNS-F10             TO        PRT-NTANNO(IX1).
*****数量
     MOVE    DNS-F13             TO        PRT-SURYO (IX1).
*
*備考
 MEISAI-EXIT.
     EXIT.
**********************************************************
*            伝票印刷
**********************************************************
 LIST-OUT-SEC                SECTION.
*印刷処理
     MOVE    "SCREEN"            TO        PRT-GRP.
     MOVE    "FVD04351"          TO        PRT-FORM.
     WRITE   PRT-FVD04351.
 LIST-OUT-EXIT.
     EXIT.
**********************************************************
*                       Ｅ Ｎ Ｄ                         *
**********************************************************
 END-SEC                   SECTION.
*
     IF   PAGE-CNT     NOT =  ZERO
          PERFORM  LIST-OUT-SEC
     END-IF.
*
     CLOSE       DNSFILF.
     CLOSE       HMEIMS.
     CLOSE       ZSOKMS.
     CLOSE       HTANMS.
     CLOSE       SUBMEIF.
     CLOSE       PRTF.
     DISPLAY  "READ-CNT=" READ-CNT UPON CONS.
     DISPLAY  "PAGE-CNT=" PAGE-CNT UPON CONS.
 END-EXIT.
     EXIT.
****************************************************************
*      1.2       入庫ファイル　ＳＴＡＲＴ処理                *
****************************************************************
 DNSFILF-START-SEC      SECTION.
     INITIALIZE                  DNS-REC.
     IF    LINK-IN-JSOK  NOT =   SPACE
*****出庫場所
           MOVE  LINK-IN-JSOK TO  DNS-F05
           MOVE  LINK-IN-SSOK TO  DNS-F05
     END-IF.
*
     START DNSFILF   KEY >=      DNS-F05  DNS-F12  DNS-F01
                                 DNS-F02
         INVALID
            MOVE   "END"     TO  END-FLG
     END-START.
 DNSFILF-START-EXIT.
     EXIT.
************************************************************
*      1.3       入庫ファイル　ＲＥＡＤ処理                *
************************************************************
 DNSFILF-READ-SEC       SECTION.
*
 DNSFILF-010.
     READ    DNSFILF
       AT  END
           MOVE     "END"        TO   END-FLG
           GO       TO   DNSFILF-READ-EXIT
     END-READ.
     ADD      1          TO   READ-CNT.
*
 DNSFILF-020.
*****出庫場所
     IF   (LINK-IN-SSOK  NOT =   SPACE  )  AND
          (LINK-IN-SSOK  NOT =   DNS-F05)
           MOVE     "END"        TO   END-FLG
           GO       TO   DNSFILF-READ-EXIT
     END-IF.
 DNSFILF-030.
*****作業日
*****IF  (LINK-IN-SAGYOST        >  DNS-F12  AND
*         LINK-IN-SAGYOEN        <  DNS-F12)
*          DISPLAY "A11" UPON CONS
*          GO       TO   DNSFILF-010
*****END-IF.
     IF  (LINK-IN-SAGYOST  <=    DNS-F12  AND
          LINK-IN-SAGYOEN  >=    DNS-F12)
           CONTINUE
     ELSE
***********DISPLAY "A11" UPON CONS
           GO       TO   DNSFILF-010
     END-IF.
 DNSFILF-040.
*
*****IF  (LINK-IN-DENNOST        >  DNS-F01  AND
*         LINK-IN-DENNOEN        <  DNS-F01)
*          DISPLAY "A12" UPON CONS
*          GO       TO   DNSFILF-010
*****END-IF.
     IF  (LINK-IN-DENNOST  <=   DNS-F01  AND
          LINK-IN-DENNOEN  >=   DNS-F01)
           CONTINUE
     ELSE
***********DISPLAY "A12" UPON CONS
           GO       TO   DNSFILF-010
     END-IF.
*
     IF  LINK-IN-HAKKBN  =  "2"
         CONTINUE
     ELSE
         IF  LINK-IN-HAKKBN  =  "1"
*************出庫明細発行区分
             IF  DNS-F26  =  "1"
                 CONTINUE
             ELSE
                 GO       TO   DNSFILF-010
             END-IF
         ELSE
             IF  DNS-F26  =  SPACE
                 CONTINUE
             ELSE
                 GO       TO   DNSFILF-010
             END-IF
         END-IF
     END-IF.
*
     MOVE  "1"                    TO   DNS-F26.
     REWRITE  DNS-REC.
*
 DNSFILF-READ-EXIT.
     EXIT.
************************************************************
*      倉庫マスタの読込
************************************************************
 ZSOKMS-READ-SEC       SECTION.
     READ    ZSOKMS
       INVALID      KEY
          MOVE      "INV"        TO   ZSOKMS-INV-FLG
       NOT INVALID      KEY
          MOVE      SPACE        TO   ZSOKMS-INV-FLG
     END-READ.
 ZSOKMS-READ-EXT.
     EXIT.
************************************************************
*      商品名称マスタの読込
************************************************************
 HMEIMS-READ-SEC       SECTION.
     READ    HMEIMS
       INVALID      KEY
          MOVE      "INV"        TO   HMEIMS-INV-FLG
       NOT INVALID      KEY
          MOVE      SPACE        TO   HMEIMS-INV-FLG
     END-READ.
 HMEIMS-READ-EXT.
     EXIT.
************************************************************
*      サブ商品名称マスタの読込
************************************************************
 SUBMEIF-READ-SEC       SECTION.
     READ    SUBMEIF
       INVALID      KEY
          MOVE      "INV"        TO   SUBMEIF-INV-FLG
       NOT INVALID      KEY
          MOVE      SPACE        TO   SUBMEIF-INV-FLG
     END-READ.
 SUBMEIF-READ-EXT.
     EXIT.
************************************************************
*      担当者マスタの読込
************************************************************
 HTANMS-READ-SEC       SECTION.
     READ    HTANMS
       INVALID      KEY
          MOVE      "INV"        TO   HTANMS-INV-FLG
       NOT INVALID      KEY
          MOVE      SPACE        TO   HTANMS-INV-FLG
     END-READ.
 HTANMS-READ-EXT.
     EXIT.

```
