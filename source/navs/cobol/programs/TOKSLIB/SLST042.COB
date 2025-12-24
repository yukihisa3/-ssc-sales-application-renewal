**********************************************************
*                                                        *
*         　　　   商品変換ＴＢＬリスト_                *
*                           S L S T 0 4 0                *
**********************************************************
 IDENTIFICATION            DIVISION.
 PROGRAM-ID.               OLST042.
 AUTHOR.                   Y.Y.
 DATE-WRITTEN.             93/03/25.
 ENVIRONMENT               DIVISION.
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
*商品変換ＴＢＬ
     SELECT     HSHOTBL    ASSIGN    TO        DA-01-VI-SHOTBL3
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      DYNAMIC
                           RECORD    KEY       STB-F01
                                               STB-F04
                                               STB-F02
                           FILE      STATUS    STB-ST.
*取引先マスタ　
     SELECT     HTOKMS     ASSIGN    TO        DA-01-VI-TOKMS2
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       TOK-F01
                           FILE      STATUS    TOK-ST.
*商品名称マスタ
     SELECT     HMEIMS     ASSIGN    TO        DA-01-VI-MEIMS1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       MEI-F011
                                               MEI-F0121
                                               MEI-F0122
                                               MEI-F0123
                           FILE      STATUS    MEI-ST.
*%* プリンター *%*
     SELECT     PRTF       ASSIGN    TO        GS-PRTF
                           DESTINATION        "PRT"
                           FORMAT              PRT-FORM
                           GROUP               PRT-GRP
                           PROCESSING          PRT-PROC
                           UNIT CONTROL        PRT-CTL
                           FILE      STATUS    PRT-ST.
*%* 表示ファイル *%*
     SELECT     DSPF       ASSIGN    TO        GS-DSPF
                           ORGANIZATION        SEQUENTIAL
                           DESTINATION        "DSP"
                           FORMAT              DSP-FORM
                           GROUP               DSP-GRP
                           PROCESSING          DSP-PROC
                           FUNCTION            PF-KEY
                           UNIT CONTROL        DSP-CONTROL
                           STATUS              DSP-ST.
******************************************************************
*             DATA                DIVISION                       *
******************************************************************
 DATA                      DIVISION.
 FILE                      SECTION.
*商品変換ＴＢＬ
 FD  HSHOTBL
     BLOCK       CONTAINS  48        RECORDS
     LABEL       RECORD    IS        STANDARD.
     COPY        HSHOTBL   OF        XFDLIB
     JOINING     STB       AS        PREFIX.
*取引先マスタ
 FD  HTOKMS
     BLOCK       CONTAINS   8        RECORDS
     LABEL       RECORD    IS        STANDARD.
     COPY        HTOKMS    OF        XFDLIB
     JOINING     TOK       AS        PREFIX.
*商品名称ＴＢＬ
 FD  HMEIMS
     BLOCK       CONTAINS  24        RECORDS
     LABEL       RECORD    IS        STANDARD.
     COPY        HMEIMS    OF        XFDLIB
     JOINING     MEI       AS        PREFIX.
*プリンター
 FD  PRTF
     LABEL       RECORD    IS        OMITTED.
     COPY        FL042L    OF        XMDLIB.
*表示ファイル
 FD  DSPF
     LABEL       RECORD    IS        OMITTED.
     COPY        FL042     OF        XMDLIB.
******************************************************************
 WORKING-STORAGE           SECTION.
******************************************************************
*%*表示パラメータ*%*
 01  FORM-PARA.
     03  DSP-FORM          PIC X(08).
     03  DSP-PROC          PIC X(02).
     03  DSP-GRP           PIC X(08).
     03  PF-KEY            PIC X(04).
     03  DSP-CONTROL.
         05  DSP-CNTRL     PIC X(04).
         05  DSP-STR-PG    PIC X(02).
     03  PRT-FORM          PIC X(08).
     03  PRT-PROC          PIC X(02).
     03  PRT-GRP           PIC X(08).
     03  PRT-CTL.
         05  PRT-CNTRL     PIC X(04).
         05  PRT-STR-PG    PIC X(02).
*
 01  FILE-STATUS.
     03  STB-ST            PIC X(02).
     03  STB-ST1           PIC X(04).
     03  TOK-ST            PIC X(02).
     03  TOK-ST1           PIC X(04).
     03  MEI-ST            PIC X(02).
     03  MEI-ST1           PIC X(04).
     03  PRT-ST            PIC X(02).
     03  PRT-ST1           PIC X(04).
     03  DSP-ST            PIC X(02).
     03  DSP-ST1           PIC X(04).
*システム日付
*01  SYS-DATE.
*    03  SYS-YY            PIC 9(02).
*    03  SYS-MM            PIC 9(02).
*    03  SYS-DD            PIC 9(02).
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
 01  WK-AREA.
     03  END-SW            PIC 9(01) VALUE     ZERO.
     03  ERR-SW            PIC 9(01) VALUE     ZERO.
     03  PAGE-CNT          PIC 9(05) VALUE     ZERO.
     03  LINE-CNT          PIC 9(05) VALUE     ZERO.
     03  WK-TORICD         PIC 9(08) VALUE     ZERO.
*\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
*   93.04.14  SYUUSEI  START  H.SUTO
 01  WK-TNABAN.
*    03  WK-TNA01          PIC X(03).
*    03  WK-TNA02          PIC X(01).
*    03  WK-TNA03          PIC X(01).
*    03  WK-TNA04          PIC X(01).
*    03  WK-TNA05          PIC X(02).
     03  WK-TNA01          PIC X(01).
     03  WK-TNA02          PIC X(01).
     03  WK-TNA03          PIC X(03).
     03  WK-TNA04          PIC X(01).
     03  WK-TNA05          PIC X(02).
*   93.04.14  SYUUSEI  END    H.SUTO
*\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
 01  IN-DATA               PIC X(01).
 01  FILE-ERR.
     03  DSP-ERR           PIC  N(10) VALUE
                        NC"画面ファイルエラー".
     03  STB-ERR           PIC N(10) VALUE
                        NC"商品変換Ｔエラー".
     03  TOK-ERR           PIC N(10) VALUE
                        NC"取引先マスタエラー".
     03  MEI-ERR           PIC N(10) VALUE
                        NC"商品名称マスタエラー".
     03  PRT-ERR           PIC N(10) VALUE
                        NC"プリンターエラー".
 01  WK-ERR.
     03  WK-ERR1                  PIC  N(12) VALUE
              NC"　　終了が開始より小さい".
     03  WK-ERR2                  PIC  N(12) VALUE
              NC"『商品変換ＴＢＬ作成中』".
     03  WK-ERR4                  PIC  N(12) VALUE
              NC"　対象データがありません".
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
******************************************************************
*             PROCEDURE           DIVISION                       *
******************************************************************
 PROCEDURE                           DIVISION.
 DECLARATIVES.
 STB-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HSHOTBL.
     DISPLAY     STB-ERR   UPON      STA.
     DISPLAY     STB-ST    UPON      STA.
*****DISPLAY     STB-ST1   UPON      STA.
     ACCEPT      IN-DATA   FROM      STA.
*****MOVE        255       TO        PROGRAM-STATUS.
     STOP        RUN.
 TOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HTOKMS.
     DISPLAY     TOK-ERR   UPON      STA.
     DISPLAY     TOK-ST    UPON      STA.
*****DISPLAY     TOK-ST1   UPON      STA.
     ACCEPT      IN-DATA   FROM      STA.
*****MOVE        255       TO        PROGRAM-STATUS.
     STOP        RUN.
 MEI-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HMEIMS.
     DISPLAY     MEI-ERR   UPON      STA.
     DISPLAY     MEI-ST    UPON      STA.
*****DISPLAY     MEI-ST1   UPON      STA.
     ACCEPT      IN-DATA   FROM      STA.
*****MOVE        255       TO        PROGRAM-STATUS.
     STOP        RUN.
 PRT-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE PRTF.
     DISPLAY     PRT-ERR   UPON      STA.
     DISPLAY     PRT-ST    UPON      STA.
*****DISPLAY     PRT-ST1   UPON      STA.
     ACCEPT      IN-DATA   FROM      STA.
     STOP        RUN.
 DSP-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE DSPF.
     DISPLAY     DSP-ERR   UPON      STA.
     DISPLAY     DSP-ST    UPON      STA.
*****DISPLAY     DSP-ST1   UPON      STA.
     ACCEPT      IN-DATA   FROM      STA.
*****MOVE        255       TO        PROGRAM-STATUS.
     STOP        RUN.
 END DECLARATIVES.
***********************************************************
*                     ＭＡＩＮ処理                        *
***********************************************************
 PROC-SEC                  SECTION.
     OPEN        I-O       DSPF.
 PROC-010.
     PERFORM     INIT-SEC.
     PERFORM     MAIN-SEC  UNTIL     END-SW = 9  OR 1
     PERFORM     END-SEC.
     IF  (END-SW = 1)
         GO               TO         PROC-010.
     CLOSE       DSPF.
     STOP        RUN.
 PROC-EXIT.
     EXIT.
**********************************************************
*                      Ｉ Ｎ Ｉ Ｔ                       *
**********************************************************
 INIT-SEC                  SECTION.
     MOVE        ZERO      TO        PAGE-CNT.
*****ACCEPT      SYS-DATE  FROM      DATE.
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
     MOVE      SYS-DATE(1:4)      TO   HEN-DATE-YYYY.
     MOVE      SYS-DATE(5:2)      TO   HEN-DATE-MM.
     MOVE      SYS-DATE(7:2)      TO   HEN-DATE-DD.
*システム日付取得
     ACCEPT    WK-TIME          FROM   TIME.
*画面表示時刻編集
     MOVE      WK-TIME(1:2)       TO   HEN-TIME-HH.
     MOVE      WK-TIME(3:2)       TO   HEN-TIME-MM.
     MOVE      WK-TIME(5:2)       TO   HEN-TIME-SS.
*
     OPEN        INPUT     HSHOTBL
                           HTOKMS
                           HMEIMS
                 OUTPUT    PRTF.
************
*帳票初期化*
************
     MOVE        SPACE     TO        FL042L.
     INITIALIZE  FL042L.
******************
*印刷処理チェック*
******************
     IF  (END-SW = 1)
         MOVE    ZERO      TO        END-SW
         PERFORM DSP-010   THRU      DSP-EXIT
       ELSE
         PERFORM DSP-SEC.
 INIT-EXIT.
     EXIT.
***********************************************************
*                     ＭＡＩＮ処理                        *
***********************************************************
 MAIN-SEC                  SECTION.
     IF  (LINE-CNT  >  60)
          PERFORM MIDA-SEC
     END-IF.
     PERFORM     MEIEDT-SEC.
     PERFORM     MEIWRT-SEC.
**********************
*取引先マスタＲＥＡＤ*
**********************
 MAIN-010.
     READ   HSHOTBL   NEXT  AT        END
          MOVE  "FL042L"    TO        PRT-FORM
          MOVE  "GRP003"    TO        PRT-GRP
          MOVE  "A001"      TO        PRT-CTL
          MOVE  "PW"        TO        PRT-PROC
          WRITE  FL042L
          MOVE    1         TO        END-SW
          GO                TO        MAIN-EXIT.
*出荷場所チェック
     IF  (STB-F04  <  KAISYU) OR (STB-F04  >  ENDSYU)
         GO                TO        MAIN-010.
*商品ＣＤチェック
     IF  (STB-F02  <  KAISHO) OR (STB-F02  >  ENDSHO)
         GO                TO        MAIN-010.
*終了取引先チェック
     IF  (STB-F01  >  ENDTOK)
          MOVE  "FL042L"    TO        PRT-FORM
          MOVE  "GRP003"    TO        PRT-GRP
          MOVE  "A001"      TO        PRT-CTL
          MOVE  "PW"        TO        PRT-PROC
          WRITE  FL042L
          MOVE    1         TO        END-SW
          MOVE    1         TO        END-SW
          GO                TO        MAIN-EXIT.
 MAIN-EXIT.
     EXIT.
**********************************************************
*                       Ｅ Ｎ Ｄ                         *
**********************************************************
 END-SEC                   SECTION.
     MOVE        SPACE     TO        ERRMSG.
     MOVE       "ERRMSG"   TO        DSP-GRP.
     PERFORM     DSP-WRT-SEC.
     CLOSE       PRTF      HSHOTBL    HTOKMS   HMEIMS.
 END-EXIT.
     EXIT.
**********************************************************
*                 見出しデータ編集書き出し               *
**********************************************************
 MIDA-SEC                  SECTION.
     MOVE        WK-Y      TO        SYSYY.
     MOVE        WK-M      TO        SYSMM.
     MOVE        WK-D      TO        SYSDD.
     ADD         1         TO        PAGE-CNT.
     MOVE        ZERO      TO        LINE-CNT.
     MOVE        ZERO      TO        WK-TORICD.
     MOVE        PAGE-CNT  TO        LPAGE.
**************
*帳票書き出し*
**************
     MOVE       "FL042L"   TO        PRT-FORM.
     MOVE       "GRP001"   TO        PRT-GRP.
     MOVE       "A000"     TO        PRT-CTL.
     MOVE       "PW"       TO        PRT-PROC.
     WRITE       FL042L.
     ADD         4         TO        LINE-CNT.
 MIDA-EXIT.
     EXIT.
**********************************************************
*                 明細情報の編集                         *
**********************************************************
 MEIEDT-SEC                  SECTION.
*取引先チェック
     IF    (WK-TORICD  =  STB-F01)
*         MOVE   SPACE     TO        TORICD  TORINM
          GO               TO        MEIEDT-010
     ELSE
          MOVE   STB-F01   TO        TORICD  WK-TORICD
          MOVE  "FL042L"   TO        PRT-FORM
          MOVE  "GRP003"   TO        PRT-GRP
          MOVE  "A001"     TO        PRT-CTL
          MOVE  "PW"       TO        PRT-PROC
          WRITE  FL042L
          ADD       1      TO        LINE-CNT
     END-IF.
*取引先ＲＥＡＤ
     MOVE        STB-F01   TO        TOK-F01.
     READ    HTOKMS
     INVALID
          MOVE   SPACE     TO        TORINM
     NOT INVALID
          MOVE   TOK-F03   TO        TORINM
     END-READ.
*
 MEIEDT-010.
     MOVE        STB-F02   TO        RYOCD.
     MOVE        STB-F03   TO        SHOCD.
     MOVE        STB-F05   TO        GENKA.
     MOVE        STB-F06   TO        BAIKA.
     MOVE        STB-F04   TO        SYUCD.
     MOVE        STB-F07   TO        BUNCD.
*\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
*   93.04.14  SYUUSEI  START  H.SUTO
*    MOVE        STB-F081  TO        WK-TNA01.
*    MOVE        "-"       TO        WK-TNA02.
*    MOVE        STB-F082  TO        WK-TNA03.
*    MOVE        "-"       TO        WK-TNA04.
*    MOVE        STB-F083  TO        WK-TNA05.
     MOVE        STB-F08(1:1)  TO        WK-TNA01.
     MOVE        "-"       TO        WK-TNA02.
     MOVE        STB-F08(2:3)  TO        WK-TNA03.
     MOVE        "-"       TO        WK-TNA04.
     MOVE        STB-F08(5:2)  TO        WK-TNA05.
     MOVE        WK-TNABAN TO        TNABAN.
*****MOVE        STB-F09   TO        ANKEI.
*****MOVE        STB-F10   TO        ZAIKO.
*仕入単価追加
     MOVE        STB-F09   TO        SITAN.
*   93.04.14  SYUUSEI  END    H.SUTO
*\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
*商品名称ＲＥＡＤ
     MOVE        STB-F03   TO        MEI-F01.
     READ    HMEIMS
     INVALID
*         MOVE   SPACE     TO        SHONM
          MOVE  ALL NC"＊" TO        SHONM
          MOVE  ALL NC"＊" TO        SHONM2
     NOT INVALID
          MOVE   MEI-F021  TO        SHONM
          MOVE   MEI-F022  TO        SHONM2
     END-READ.
 MEIEDT-EXIT.
     EXIT.
**********************************************************
*                    明細データ書き出し                  *
**********************************************************
 MEIWRT-SEC                   SECTION.
**************
*帳票書き出し*
**************
     MOVE       "A001"     TO        PRT-CTL.
     MOVE       "PW"       TO        PRT-PROC.
     MOVE       "GRP002"   TO        PRT-GRP.
     MOVE       "FL042L"   TO        PRT-FORM.
     WRITE       FL042L.
*帳票初期化*
     MOVE        SPACE     TO        FL042L.
     INITIALIZE  FL042L.
     ADD         3         TO        LINE-CNT.
 MEIWRT-EXIT.
     EXIT.
***********************************************************
*                       画面処理                          *
***********************************************************
 DSP-SEC                   SECTION.
     PERFORM     DSP-INT-SEC.
 DSP-010.
     MOVE       "GRP001"   TO        DSP-GRP.
     PERFORM     DSP-RD-SEC.
     EVALUATE    PF-KEY
       WHEN
        "F005"
           MOVE  9         TO        END-SW
           GO              TO        DSP-EXIT
       WHEN
        "E000"
           CONTINUE
       WHEN
         OTHER
           GO              TO        DSP-010
     END-EVALUATE.
 DSP-020.
****************
*エラーチェック*
****************
*
     IF  (ENDTOK = ZERO)
         MOVE ALL "9"      TO        ENDTOK.
     IF  (ENDSYU = SPACE)
         MOVE ALL "9"      TO        ENDSYU.
     IF  (ENDSHO = SPACE)
         MOVE ALL "9"      TO        ENDSHO.
*得意先コード大小チェック
     IF  (KAITOK > ENDTOK)
         MOVE    1         TO        ERR-SW
         MOVE   "R"        TO        EDIT-OPTION  OF    KAITOK
         MOVE   "R"        TO        EDIT-OPTION  OF    ENDTOK
         MOVE   "C"        TO        EDIT-CURSOR  OF    KAITOK
         MOVE    WK-ERR1   TO        ERRMSG
       ELSE
         MOVE   "M"        TO        EDIT-OPTION  OF    KAITOK
         MOVE   "M"        TO        EDIT-OPTION  OF    ENDTOK
         MOVE    SPACE     TO        EDIT-CURSOR  OF    KAITOK.
*
     IF  (KAISYU > ENDSYU)
         MOVE    1         TO        ERR-SW
         MOVE   "R"        TO        EDIT-OPTION  OF    KAISYU
         MOVE   "R"        TO        EDIT-OPTION  OF    ENDSYU
         MOVE   "C"        TO        EDIT-CURSOR  OF    KAISYU
         MOVE    WK-ERR1   TO        ERRMSG
       ELSE
         MOVE   "M"        TO        EDIT-OPTION  OF    KAISYU
         MOVE   "M"        TO        EDIT-OPTION  OF    ENDSYU
         MOVE    SPACE     TO        EDIT-CURSOR  OF    KAISYU.
*
     IF  (KAISHO > ENDSHO)
         MOVE    1         TO        ERR-SW
         MOVE   "R"        TO        EDIT-OPTION  OF    KAISHO
         MOVE   "R"        TO        EDIT-OPTION  OF    ENDSHO
         MOVE   "C"        TO        EDIT-CURSOR  OF    KAISHO
         MOVE    WK-ERR1   TO        ERRMSG
       ELSE
         MOVE   "M"        TO        EDIT-OPTION  OF    KAISHO
         MOVE   "M"        TO        EDIT-OPTION  OF    ENDSHO
         MOVE    SPACE     TO        EDIT-CURSOR  OF    KAISHO.
*項目表示
     MOVE       "GRP001"   TO        DSP-GRP.
     PERFORM     DSP-WRT-SEC.
*エラー存在？
     IF  (ERR-SW = 1)
         MOVE    ZERO      TO        ERR-SW
         MOVE   "ERRMSG"   TO        DSP-GRP
         PERFORM DSP-WRT-SEC
         GO                TO        DSP-010.
*エラーメッセージ空白
     MOVE        SPACE     TO        ERRMSG.
     MOVE       "ERRMSG"   TO        DSP-GRP.
     PERFORM     DSP-WRT-SEC.
 DSP-030.
     MOVE       "R001"     TO        DSP-GRP.
     PERFORM     DSP-RD-SEC.
     EVALUATE    PF-KEY
       WHEN
        "F005"
           MOVE  9         TO        END-SW
           GO              TO        DSP-EXIT
       WHEN
        "F004"
           MOVE  ZERO      TO        ERR-SW
           GO              TO        DSP-SEC
       WHEN
        "E000"
           CONTINUE
       WHEN
         OTHER
           GO              TO        DSP-030
     END-EVALUATE.
**********************
*取引先マスタＲＥＡＤ*
**********************
 DSP-040.
     MOVE        KAITOK    TO        STB-F01.
     MOVE        KAISYU    TO        STB-F04.
     MOVE        KAISHO    TO        STB-F02.
     START       HSHOTBL   KEY  IS   >=
                 STB-F01   STB-F04   STB-F02
       INVALID
         MOVE    WK-ERR4   TO        ERRMSG
         MOVE   "ERRMSG"   TO        DSP-GRP
         PERFORM DSP-WRT-SEC
         MOVE    ZERO      TO        ERR-SW
         GO                TO        DSP-010.
 DSP-050.
     READ   HSHOTBL   NEXT  AT        END
         MOVE    WK-ERR4   TO        ERRMSG
         MOVE   "ERRMSG"   TO        DSP-GRP
         PERFORM DSP-WRT-SEC
         MOVE    ZERO      TO        ERR-SW
         GO                TO        DSP-010.
*出荷場所チェック
     IF  (STB-F04  <  KAISYU) OR (STB-F04  >  ENDSYU)
         GO                TO        DSP-050.
*商品ＣＤチェック
     IF  (STB-F02  <  KAISHO) OR (STB-F02  >  ENDSHO)
         GO                TO        DSP-050.
*終了取引先チェック
     IF  (STB-F01  >  ENDTOK)
         MOVE    WK-ERR4   TO        ERRMSG
         MOVE   "ERRMSG"   TO        DSP-GRP
         PERFORM DSP-WRT-SEC
         MOVE    ZERO      TO        ERR-SW
         GO                TO        DSP-010.
*印字メッセージ出力
 DSP-060.
     MOVE        WK-ERR2   TO        ERRMSG.
     MOVE       "ERRMSG"   TO        DSP-GRP.
     PERFORM     DSP-WRT-SEC.
     PERFORM     MIDA-SEC.
     MOVE        ZERO      TO        WK-TORICD.
 DSP-EXIT.
     EXIT.
***********************************************************
*                       画面出力                          *
***********************************************************
 DSP-WRT-SEC               SECTION.
     MOVE        SPACE     TO        DSP-PROC.
     WRITE       FL042.
     IF  ( DSP-ST NOT = "00" )
         STOP RUN.
 DSP-WRT-EXIT.
     EXIT.
***********************************************************
*                      画面入力                           *
***********************************************************
 DSP-RD-SEC                SECTION.
     MOVE       "NE"       TO        DSP-PROC.
     READ        DSPF      AT        END
                 STOP      RUN.
 DSP-RD-EXIT.
     EXIT.
***********************************************************
*                    画面初期設定                         *
***********************************************************
 DSP-INT-SEC               SECTION.
     MOVE        SPACE     TO        FL042.
     MOVE       "SCREEN"   TO        DSP-GRP.
     MOVE       "FL042"    TO        DSP-FORM.
     MOVE       "CL"       TO        DSP-PROC.
     MOVE        HEN-DATE  TO        SDATE.
     MOVE        HEN-TIME  TO        STIME.
     PERFORM     DSP-WRT-SEC.
     INITIALIZE  FL042.
 DSP-INT-EXIT.
     EXIT.
