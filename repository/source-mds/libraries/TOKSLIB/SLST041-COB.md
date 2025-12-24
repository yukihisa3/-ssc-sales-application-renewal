# SLST041

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SLST041.COB`

## ソースコード

```cobol
**********************************************************
*                                                        *
*         　　　   商品変換ＴＢＬリスト_                *
*                           S L S T 0 4 0                *
**********************************************************
* 仕様変更  93/10/27                                     *
* 概要      範囲指定入力の商品コードを　                 *
*           X(16) => X(08) X(05) X(02) X(01) に分割      *
* 変更履歴：94/12/01 T.TAKAHASHI                         *
**********************************************************
 IDENTIFICATION            DIVISION.
 PROGRAM-ID.               OLST041.
 AUTHOR.                   Y.Y.
 DATE-WRITTEN.             92/12/02.
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
     SELECT     HSHOTBL    ASSIGN    TO        DA-01-VI-SHOTBL2
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      DYNAMIC
                           RECORD    KEY       STB-F01
                                               STB-F04
                                               STB-F031
                                               STB-F0321
                                               STB-F0322
                                               STB-F0323
                                     WITH      DUPLICATES
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
     COPY        FL041L    OF        XMDLIB.
*表示ファイル
 FD  DSPF
     LABEL       RECORD    IS        OMITTED.
     COPY        FL041     OF        XMDLIB.
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
     03  HEN-FLG           PIC 9(01) VALUE     ZERO.
     03  I                 PIC 9(01) VALUE     ZERO.
     03  J                 PIC 9(01) VALUE     ZERO.
 01  WK-SHOCD-AREA.
     03  WK-SHOCDI-X.
         05  WK-SHOCDI        PIC  X(01)  OCCURS  8.
     03  WK-SHOCDO-X.
         05  WK-SHOCDO        PIC  X(01)  OCCURS  8.
     03  WK-SHOCD             PIC  9(8).
     03  WK-KAISHI.
       05  WK-KAISHO       PIC X(08).
       05  WK-KAISH5       PIC X(05).
       05  WK-KAISH2       PIC X(02).
       05  WK-KAISH1       PIC X(01).
     03  WK-SYURYO.
       05  WK-ENDSHO       PIC X(08).
       05  WK-ENDSH5       PIC X(05).
       05  WK-ENDSH2       PIC X(02).
       05  WK-ENDSH1       PIC X(01).
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
     MOVE        SPACE     TO        FL041L.
     INITIALIZE  FL041L.
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
*
*TEST
******DISPLAY   "SSSS  = "  STB-F02 UPON STA.
     IF  (LINE-CNT  >  60)
          PERFORM MIDA-SEC
     END-IF.
     PERFORM     MEIEDT-SEC.
     PERFORM     MEIWRT-SEC.
**********************
*商品変換ＴＢＬマスタＲＥＡＤ*
**********************
 MAIN-010.
     READ   HSHOTBL   NEXT  AT        END
          MOVE  "FL041L"    TO        PRT-FORM
          MOVE  "GRP003"    TO        PRT-GRP
          MOVE  "A001"      TO        PRT-CTL
          MOVE  "PW"        TO        PRT-PROC
          WRITE  FL041L
          MOVE    1         TO        END-SW
          GO                TO        MAIN-EXIT.
*終了取引先チェック
     IF  (STB-F01  >  ENDTOK)
          MOVE  "FL041L"    TO        PRT-FORM
          MOVE  "GRP003"    TO        PRT-GRP
          MOVE  "A001"      TO        PRT-CTL
          MOVE  "PW"        TO        PRT-PROC
          WRITE  FL041L
          MOVE    1         TO        END-SW
          MOVE    1         TO        END-SW
          GO                TO        MAIN-EXIT.
*出荷場所チェック
     IF  STB-F04  <  KAISYU
         GO                TO        MAIN-010.
     IF  STB-F04  >  ENDSYU
         GO                TO        MAIN-010.
*商品ＣＤチェック
*** NAV･ASSIST 94/11/25 T.T START ***
     IF  STB-F03  <  WK-KAISHI
         GO                TO        MAIN-010.
     IF  STB-F03  >  WK-SYURYO
         GO                TO        MAIN-010.
*****IF  STB-F03  <  WK-KAISHO
*****    GO                TO        MAIN-010.
*****IF  STB-F03  >  WK-ENDSHO
*****    GO                TO        MAIN-010.
*** NAV･ASSIST 94/11/25 T.T END   ***
 MAIN-EXIT.
     EXIT.
**********************************************************
*                       Ｅ Ｎ Ｄ                         *
**********************************************************
 END-SEC                   SECTION.
**** IF  (LINE-CNT  >  60)
****      PERFORM MIDA-SEC
**** END-IF.
**** PERFORM     MEIEDT-SEC.
*****PERFORM     MEIWRT-SEC.
*
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
     MOVE       "FL041L"   TO        PRT-FORM.
     MOVE       "GRP001"   TO        PRT-GRP.
     MOVE       "A000"     TO        PRT-CTL.
     MOVE       "PW"       TO        PRT-PROC.
     WRITE       FL041L.
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
          MOVE  "FL041L"   TO        PRT-FORM
          MOVE  "GRP003"   TO        PRT-GRP
          MOVE  "A001"     TO        PRT-CTL
          MOVE  "PW"       TO        PRT-PROC
          WRITE  FL041L
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
     MOVE       "FL041L"   TO        PRT-FORM.
     WRITE       FL041L.
*帳票初期化*
     MOVE        SPACE     TO        FL041L.
     INITIALIZE  FL041L.
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
     IF  (ENDSH5 = SPACE)
         MOVE ALL "9"      TO        ENDSH5.
     IF  (ENDSH2 = SPACE)
         MOVE ALL "9"      TO        ENDSH2.
     IF  (ENDSH1 = SPACE)
         MOVE ALL "9"      TO        ENDSH1.
*商品ＣＤ数字変換
*開始商品コード
     IF  KAISHO NOT NUMERIC
     AND KAISHO NOT = SPACE
         MOVE   KAISHO  TO  WK-SHOCDI-X
         MOVE   ZERO       TO  J  HEN-FLG
         PERFORM VARYING I FROM 1 BY 1 UNTIL I > 8
                                          OR HEN-FLG NOT = 0
           IF  WK-SHOCDI(I)  NUMERIC
               ADD    1   TO  J
               MOVE   WK-SHOCDI(I)  TO  WK-SHOCDO(J)
           ELSE
             IF  WK-SHOCDI(I)  NOT = SPACE
               MOVE   1   TO  HEN-FLG
             END-IF
           END-IF
         END-PERFORM
         IF  HEN-FLG = ZERO
           MOVE   ZERO             TO  WK-SHOCD
           COMPUTE I = 9 - J
           MOVE   WK-SHOCDO-X(1:J)  TO  WK-SHOCD(I:J)
           MOVE   WK-SHOCD          TO  KAISHO
         END-IF
     END-IF.
*終了商品コード
     IF  ENDSHO NOT NUMERIC
     AND ENDSHO NOT = SPACE
         MOVE   ENDSHO  TO  WK-SHOCDI-X
         MOVE   ZERO       TO  J  HEN-FLG
         PERFORM VARYING I FROM 1 BY 1 UNTIL I > 8
                                          OR HEN-FLG NOT = 0
           IF  WK-SHOCDI(I)  NUMERIC
               ADD    1   TO  J
               MOVE   WK-SHOCDI(I)  TO  WK-SHOCDO(J)
           ELSE
             IF  WK-SHOCDI(I)  NOT = SPACE
               MOVE   1   TO  HEN-FLG
             END-IF
           END-IF
         END-PERFORM
         IF  HEN-FLG = ZERO
           MOVE   ZERO             TO  WK-SHOCD
           COMPUTE I = 9 - J
           MOVE   WK-SHOCDO-X(1:J)  TO  WK-SHOCD(I:J)
           MOVE   WK-SHOCD          TO  ENDSHO
         END-IF
     END-IF.
*
     MOVE    KAISHO        TO        WK-KAISHO.
     MOVE    KAISH5        TO        WK-KAISH5.
     MOVE    KAISH2        TO        WK-KAISH2.
     MOVE    KAISH1        TO        WK-KAISH1.
     MOVE    ENDSHO        TO        WK-ENDSHO.
     MOVE    ENDSH5        TO        WK-ENDSH5.
     MOVE    ENDSH2        TO        WK-ENDSH2.
     MOVE    ENDSH1        TO        WK-ENDSH1.
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
     IF  (WK-KAISHI        >         WK-SYURYO )
*****IF  (KAISHO > ENDSHO)
         MOVE    1         TO        ERR-SW
         MOVE   "R"        TO        EDIT-OPTION  OF    KAISHO
         MOVE   "R"        TO        EDIT-OPTION  OF    KAISH5
         MOVE   "R"        TO        EDIT-OPTION  OF    KAISH2
         MOVE   "R"        TO        EDIT-OPTION  OF    KAISH1
         MOVE   "R"        TO        EDIT-OPTION  OF    ENDSHO
         MOVE   "R"        TO        EDIT-OPTION  OF    ENDSH5
         MOVE   "R"        TO        EDIT-OPTION  OF    ENDSH2
         MOVE   "R"        TO        EDIT-OPTION  OF    ENDSH1
         MOVE   "C"        TO        EDIT-CURSOR  OF    KAISHO
         MOVE    WK-ERR1   TO        ERRMSG
       ELSE
         MOVE   "M"        TO        EDIT-OPTION  OF    KAISHO
         MOVE   "M"        TO        EDIT-OPTION  OF    KAISH5
         MOVE   "M"        TO        EDIT-OPTION  OF    KAISH2
         MOVE   "M"        TO        EDIT-OPTION  OF    KAISH1
         MOVE   "M"        TO        EDIT-OPTION  OF    ENDSHO
         MOVE   "M"        TO        EDIT-OPTION  OF    ENDSH5
         MOVE   "M"        TO        EDIT-OPTION  OF    ENDSH2
         MOVE   "M"        TO        EDIT-OPTION  OF    ENDSH1
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
*商品変換ＴＢＬマスタＲＥＡＤ*
**********************
 DSP-040.
     MOVE        KAITOK    TO        STB-F01.
     MOVE        KAISYU    TO        STB-F04.
     MOVE        WK-KAISHI TO        STB-F03.
     START       HSHOTBL    KEY  IS   >=
            STB-F01 STB-F04 STB-F031 STB-F0321 STB-F0322 STB-F0323
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
*終了取引先チェック
     IF  (STB-F01  >  ENDTOK)
         MOVE    WK-ERR4   TO        ERRMSG
         MOVE   "ERRMSG"   TO        DSP-GRP
         PERFORM DSP-WRT-SEC
         MOVE    ZERO      TO        ERR-SW
         GO                TO        DSP-010.
*出荷場所チェック
     IF  (STB-F04  <  KAISYU) OR (STB-F04  >  ENDSYU)
         GO                TO        DSP-050.
*商品ＣＤチェック
     IF  (STB-F03  <  WK-KAISHI )    OR
         (STB-F03  >  WK-SYURYO )
         GO                TO        DSP-050.
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
     WRITE       FL041.
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
     MOVE        SPACE     TO        FL041.
     MOVE       "SCREEN"   TO        DSP-GRP.
     MOVE       "FL041"    TO        DSP-FORM.
     MOVE       "CL"       TO        DSP-PROC.
     MOVE        HEN-DATE  TO        SDATE.
     MOVE        HEN-TIME  TO        STIME.
     PERFORM     DSP-WRT-SEC.
     INITIALIZE  FL041.
 DSP-INT-EXIT.
     EXIT.

```
