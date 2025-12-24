# SNI0040L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SNI0040L.COB`

## ソースコード

```cobol
**********************************************************
*                                                        *
*         　　売上データ確認リスト                       *
*                           SNI0040L                     *
**********************************************************
 IDENTIFICATION            DIVISION.
 PROGRAM-ID.               SNI0040L.
 AUTHOR.                   Y.Y.
 DATE-WRITTEN.             92/12/09.
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
*
     SELECT     TOKU       ASSIGN    TO        DA-01-VI-TOKU1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      SEQUENTIAL
                           RECORD    KEY       TKU-F06
                                               TKU-F08
                                               TKU-F09
                                               TKU-F10
                           FILE      STATUS    TEN-ST.
*取引先マスタ
     SELECT     HTOKMS     ASSIGN    TO        DA-01-VI-TOKMS3
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       TOK-F52
                           FILE      STATUS    TOK-ST.
*%* プリンター *%*
     SELECT     PRTF       ASSIGN    TO        01-GS-PRTF
                           DESTINATION        "PRT"
                           FORMAT              PRT-FORM
                           GROUP               PRT-GRP
                           PROCESSING          PRT-PROC
                           UNIT CONTROL        PRT-CTL
                           FILE      STATUS    PRT-ST.
******************************************************************
*             DATA                DIVISION                       *
******************************************************************
 DATA                      DIVISION.
 FILE                      SECTION.
*
 FD  TOKU
     BLOCK       CONTAINS  16        RECORDS
     LABEL       RECORD    IS        STANDARD.
     COPY        TOKU      OF        XFDLIB
     JOINING     TKU       AS        PREFIX.
*取引先マスタ
 FD  HTOKMS
     BLOCK       CONTAINS   8        RECORDS
     LABEL       RECORD    IS        STANDARD.
     COPY        HTOKMS    OF        XFDLIB
     JOINING     TOK       AS        PREFIX.
*プリンター
 FD  PRTF
     LABEL       RECORD    IS        OMITTED.
     COPY        FNI00401     OF        XMDLIB.
******************************************************************
 WORKING-STORAGE           SECTION.
******************************************************************
*%*表示パラメータ*%*
 01  FORM-PARA.
     03  PRT-FORM          PIC X(08).
     03  PRT-PROC          PIC X(02).
     03  PRT-GRP           PIC X(08).
     03  PRT-CTL.
         05  PRT-CNTRL     PIC X(04).
         05  PRT-STR-PG    PIC X(02).
*
 01  FILE-STATUS.
     03  TEN-ST            PIC X(02).
     03  TEN-ST1           PIC X(04).
     03  TOK-ST            PIC X(02).
     03  TOK-ST1           PIC X(04).
     03  PRT-ST            PIC X(02).
     03  PRT-ST1           PIC X(04).
     03  IN-DATA           PIC X(01).
*システム日付
 01  SYS-DATE              PIC  9(06).
 01  WK-AREA.
     03  END-SW            PIC  9(01) VALUE     ZERO.
     03  ERR-SW            PIC  9(01) VALUE     ZERO.
     03  PAGE-CNT          PIC  9(05) VALUE     ZERO.
     03  LINE-CNT          PIC  9(05) VALUE     ZERO.
*ブレイクキー
     03  WK-TORICD         PIC  X(08) VALUE     SPACE.
     03  WK-CYUMON         PIC  9(08) VALUE     ZERO.
     03  WK-NOUHIN         PIC  9(08) VALUE     ZERO.
     03  WK-SYUKA          PIC  9(08) VALUE     ZERO.
*明細用
     03  WK-KEN            PIC  9(06) VALUE     ZERO.
     03  WK-SUU            PIC S9(10) VALUE     ZERO.
     03  WK-KIN            PIC S9(10) VALUE     ZERO.
*小計用
     03  WS-KEN            PIC  9(06) VALUE     ZERO.
     03  WS-SUU            PIC S9(10) VALUE     ZERO.
     03  WS-KIN            PIC S9(10) VALUE     ZERO.
*合計用
     03  WG-KEN            PIC  9(06) VALUE     ZERO.
     03  WG-SUU            PIC S9(10) VALUE     ZERO.
     03  WG-KIN            PIC S9(10) VALUE     ZERO.
 01  FILE-ERR.
     03  TEN-ERR           PIC  N(10) VALUE
                        NC"ＴＯＫＵエラー".
     03  TOK-ERR           PIC  N(10) VALUE
                        NC"取引先マスタエラー".
     03  PRT-ERR           PIC  N(10) VALUE
                        NC"プリンターエラー".
******************************************************************
*             PROCEDURE           DIVISION                       *
******************************************************************
 PROCEDURE                           DIVISION.
 DECLARATIVES.
 TEN-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE TOKU.
     DISPLAY     TEN-ERR   UPON      STA.
     DISPLAY     TEN-ST    UPON      STA.
     ACCEPT      IN-DATA   FROM      STA.
*****MOVE        255       TO        PROGRAM-STATUS.
     STOP        RUN.
 TOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HTOKMS.
     DISPLAY     TOK-ERR   UPON      STA.
     DISPLAY     TOK-ST    UPON      STA.
     ACCEPT      IN-DATA   FROM      STA.
*****MOVE        255       TO        PROGRAM-STATUS.
     STOP        RUN.
 PRT-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE PRTF.
     DISPLAY     PRT-ERR   UPON      STA.
     DISPLAY     PRT-ST    UPON      STA.
     ACCEPT      IN-DATA   FROM      STA.
     STOP        RUN.
 END DECLARATIVES.
***********************************************************
*                     ＭＡＩＮ処理                        *
***********************************************************
 PROC-SEC                  SECTION.
     PERFORM     INIT-SEC.
     PERFORM     MAIN-SEC  UNTIL     END-SW = 9.
     PERFORM     END-SEC.
     STOP        RUN.
 PROC-EXIT.
     EXIT.
**********************************************************
*                      Ｉ Ｎ Ｉ Ｔ                       *
**********************************************************
 INIT-SEC               SECTION.
     MOVE     ZERO           TO        PAGE-CNT.
     ACCEPT   SYS-DATE       FROM      DATE.
     OPEN     INPUT          TOKU
                             HTOKMS
              OUTPUT         PRTF.
************
*帳票初期化*
************
     MOVE     SPACE          TO        FNI00401.
     INITIALIZE  FNI00401.
**************
*初期ＲＥＡＤ*
**************
     PERFORM  READ-SEC.
     IF  (END-SW  NOT =  9)
          MOVE     TKU-F06   TO        WK-TORICD
          MOVE     TKU-F08   TO        WK-CYUMON
          MOVE     TKU-F09   TO        WK-NOUHIN
          MOVE     TKU-F10   TO        WK-SYUKA
          PERFORM  MIDA-SEC
          PERFORM  MEIEDT-SEC.
 INIT-EXIT.
     EXIT.
***********************************************************
*                     ＭＡＩＮ処理                        *
***********************************************************
 MAIN-SEC                  SECTION.
     IF  (LINE-CNT  >  60)
          PERFORM  MIDA-SEC.
************
*小計ＣＨＫ*
************
     IF  (WK-TORICD  NOT =  TKU-F06)
      OR (WK-CYUMON  NOT =  TKU-F08)
      OR (WK-NOUHIN  NOT =  TKU-F09)
          PERFORM  MEIWRT-SEC
          PERFORM  SHOKEI-SEC
          PERFORM  MEIEDT-SEC.
*
     IF  (WK-SYUKA  NOT =  TKU-F10)
          PERFORM  MEIWRT-SEC
          PERFORM  MEIEDT-SEC.
*
     ADD      1            TO     WK-KEN.
     PERFORM  KEISAN-SEC.
**********
*ＲＥＡＤ*
**********
     PERFORM  READ-SEC.
 MAIN-EXIT.
     EXIT.
**********************************************************
*                       Ｅ Ｎ Ｄ                         *
**********************************************************
 END-SEC                   SECTION.
*
     IF  (PAGE-CNT  NOT =  0)
          IF  (LINE-CNT  >  60)
               PERFORM  MIDA-SEC
          END-IF
          PERFORM  MEIWRT-SEC
          IF  (LINE-CNT  >  60)
               PERFORM  MIDA-SEC
          END-IF
          PERFORM  SHOKEI-SEC
          IF  (LINE-CNT  >  60)
               PERFORM  MIDA-SEC
          END-IF
          PERFORM  GOKEI-SEC.
     CLOSE    TOKU      PRTF      HTOKMS.
 END-EXIT.
     EXIT.
**********************************************************
*                  Ｒ Ｅ Ａ Ｄ                     *
**********************************************************
 READ-SEC                  SECTION.
*
     READ     TOKU   AT  END
          MOVE     9     TO       END-SW.
 READ-EXIT.
     EXIT.
**********************************************************
*                 見出しデータ編集書き出し               *
**********************************************************
 MIDA-SEC                  SECTION.
     MOVE        SYS-DATE  TO        SDATE.
     ADD         1         TO        PAGE-CNT.
     MOVE        ZERO      TO        LINE-CNT.
     MOVE        PAGE-CNT  TO        LPAGE.
**************
*帳票書き出し*
**************
     MOVE       "FNI00401"    TO        PRT-FORM.
     MOVE       "GRP001"   TO        PRT-GRP.
     MOVE       "A000"     TO        PRT-CTL.
     MOVE       "PW"       TO        PRT-PROC.
     WRITE       FNI00401.
     ADD         5         TO        LINE-CNT.
 MIDA-EXIT.
     EXIT.
**********************************************************
*                 明細情報の編集                         *
**********************************************************
 MEIEDT-SEC                  SECTION.
     IF    (WS-KEN  NOT =  ZERO)
          MOVE   SPACE     TO        TORICD  TORINM
          GO               TO        MEIEDT-010.
*取引先ＲＥＡＤ
     MOVE        TKU-F06   TO        TOK-F52.
     READ    HTOKMS
     INVALID
          MOVE   ALL "*"   TO        TORICD
     NOT INVALID
          MOVE   TOK-F01   TO        TORICD
          MOVE   TOK-F03   TO        TORINM
     END-READ.
*
 MEIEDT-010.
     MOVE        TKU-F08   TO        CYUMON.
     MOVE        TKU-F09   TO        NOUHIN.
     MOVE        TKU-F10   TO        SYUKA.
 MEIEDT-EXIT.
     EXIT.
**********************************************************
*                    明細データ書き出し                  *
**********************************************************
 MEIWRT-SEC                   SECTION.
     MOVE        WK-KEN    TO        KEN.
     MOVE        WK-SUU    TO        SUU.
     MOVE        WK-KIN    TO        KIN.
*
**************
*帳票書き出し*
**************
     MOVE       "A001"     TO        PRT-CTL.
     MOVE       "PW"       TO        PRT-PROC.
     MOVE       "GRP002"   TO        PRT-GRP.
     MOVE       "FNI00401"    TO        PRT-FORM.
     WRITE       FNI00401.
*帳票初期化*
     MOVE        SPACE     TO        FNI00401.
     INITIALIZE  FNI00401.
     ADD         1         TO        LINE-CNT.
*
     MOVE        TKU-F06   TO        WK-TORICD.
     MOVE        TKU-F08   TO        WK-CYUMON.
     MOVE        TKU-F09   TO        WK-NOUHIN.
     MOVE        TKU-F10   TO        WK-SYUKA.
*
     ADD         WK-KEN    TO        WS-KEN.
     ADD         WK-SUU    TO        WS-SUU.
     ADD         WK-KIN    TO        WS-KIN.
*
     MOVE        ZERO      TO        WK-KEN.
     MOVE        ZERO      TO        WK-SUU.
     MOVE        ZERO      TO        WK-KIN.
 MEIWRT-EXIT.
     EXIT.
**********************************************************
*                 小計データ編集書き出し               *
**********************************************************
 SHOKEI-SEC                SECTION.
     MOVE        WS-KEN    TO        SKEN.
     MOVE        WS-SUU    TO        SSUU.
     MOVE        WS-KIN    TO        SKIN.
**************
*帳票書き出し*
**************
     MOVE       "FNI00401"    TO        PRT-FORM.
     MOVE       "GRP003"   TO        PRT-GRP.
     MOVE       "A001"     TO        PRT-CTL.
     MOVE       "PW"       TO        PRT-PROC.
     WRITE       FNI00401.
     ADD         2         TO        LINE-CNT.
*
     ADD         WS-KEN    TO        WG-KEN.
     ADD         WS-SUU    TO        WG-SUU.
     ADD         WS-KIN    TO        WG-KIN.
*
     MOVE        ZERO      TO        WS-KEN.
     MOVE        ZERO      TO        WS-SUU.
     MOVE        ZERO      TO        WS-KIN.
 SHOKEI-EXIT.
     EXIT.
**********************************************************
*              総合計データ編集書き出し               *
**********************************************************
 GOKEI-SEC                SECTION.
     MOVE        WG-KEN    TO        GKEN.
     MOVE        WG-SUU    TO        GSUU.
     MOVE        WG-KIN    TO        GKIN.
**************
*帳票書き出し*
**************
     MOVE       "FNI00401"    TO        PRT-FORM.
     MOVE       "GRP004"   TO        PRT-GRP.
     MOVE       "A001"     TO        PRT-CTL.
     MOVE       "PW"       TO        PRT-PROC.
     WRITE       FNI00401.
 GOKEI-EXIT.
     EXIT.
**********************************************************
*              数量，金額　計算　　　　               *
**********************************************************
 KEISAN-SEC                SECTION.
     EVALUATE    TKU-F05
*偶数処理
       WHEN   0
       WHEN   2
       WHEN   4
       WHEN   6
       WHEN   8
           EVALUATE    TKU-F02
              WHEN   40
              WHEN   45
                  COMPUTE   WK-SUU  =  WK-SUU  +  TKU-F15
                  COMPUTE   WK-KIN  =  WK-KIN  +  TKU-F17
              WHEN   41
              WHEN   42
              WHEN   44
                  COMPUTE   WK-SUU  =  WK-SUU  -  TKU-F15
                  COMPUTE   WK-KIN  =  WK-KIN  -  TKU-F17
           END-EVALUATE
*奇数処理
       WHEN   1
       WHEN   3
       WHEN   5
       WHEN   7
       WHEN   9
           EVALUATE    TKU-F02
              WHEN   40
              WHEN   45
                  COMPUTE   WK-SUU  =  WK-SUU  -  TKU-F15
                  COMPUTE   WK-KIN  =  WK-KIN  -  TKU-F17
              WHEN   41
              WHEN   42
              WHEN   44
                  COMPUTE   WK-SUU  =  WK-SUU  +  TKU-F15
                  COMPUTE   WK-KIN  =  WK-KIN  +  TKU-F17
           END-EVALUATE
     END-EVALUATE.
 GOKEI-EXIT.
     EXIT.

```
