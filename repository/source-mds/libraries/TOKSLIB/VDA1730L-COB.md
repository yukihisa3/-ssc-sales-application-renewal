# VDA1730L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/VDA1730L.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　：　サカタのタネ（株）　　　　　　　　　　*
*    業務名　　　　　：　マスタメンテ                          *
*    モジュール名　　：　店舗別ルートマスタリスト　　　　　　　*
*    作成日／更新日　：　97/03/17                              *
*    作成者／更新者　：　NAV･ASSIST                            *
*    更新日／更新者　：                                        *
*                                                              *
****************************************************************
 IDENTIFICATION            DIVISION.
 PROGRAM-ID.               VDA1730L.
 AUTHOR.                   Y.Y.
 DATE-WRITTEN.             97/03/17.
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
*店舗別ルートマスタ
     SELECT     HTENRMS    ASSIGN    TO        DA-01-VI-TENRMS2
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      DYNAMIC
                           RECORD    KEY       TER-F01
                                               TER-F02
                                               TER-F04
                                               TER-F05
                                               TER-F03
                           FILE      STATUS    TER-ST.
*店舗マスタ
     SELECT     HTENMS     ASSIGN    TO        DA-01-VI-TENMS1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       TEN-F52
                                               TEN-F011
                           FILE      STATUS    TEN-ST.
*取引先マスタ
     SELECT     HTOKMS     ASSIGN    TO        DA-01-VI-TOKMS2
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       TOK-F01
                           FILE      STATUS    TOK-ST.
*倉庫マスタ
     SELECT     ZSOKMS     ASSIGN    TO        DA-01-VI-ZSOKMS1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       SOK-F01
                           FILE      STATUS    SOK-ST.
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
*店舗別ルートマスタ
 FD  HTENRMS
     LABEL       RECORD    IS        STANDARD.
     COPY        HTENRMS   OF        XFDLIB
     JOINING     TER       AS        PREFIX.
*店舗マスタ
 FD  HTENMS
     LABEL       RECORD    IS        STANDARD.
     COPY        HTENMS    OF        XFDLIB
     JOINING     TEN       AS        PREFIX.
*取引先マスタ
 FD  HTOKMS
     LABEL       RECORD    IS        STANDARD.
     COPY        HTOKMS    OF        XFDLIB
     JOINING     TOK       AS        PREFIX.
*倉庫マスタ
 FD  ZSOKMS
     LABEL       RECORD    IS        STANDARD.
     COPY        ZSOKMS    OF        XFDLIB
     JOINING     SOK       AS        PREFIX.
*プリンター
 FD  PRTF
     LABEL       RECORD    IS        OMITTED.
     COPY        FVDA1731  OF        XMDLIB.
*表示ファイル
 FD  DSPF
     LABEL       RECORD    IS        OMITTED.
     COPY        FVDA1730  OF        XMDLIB.
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
     03  TER-ST            PIC X(02).
     03  TER-ST1           PIC X(04).
     03  TEN-ST            PIC X(02).
     03  TEN-ST1           PIC X(04).
     03  TOK-ST            PIC X(02).
     03  TOK-ST1           PIC X(04).
     03  SOK-ST            PIC X(02).
     03  SOK-ST1           PIC X(04).
     03  PRT-ST            PIC X(02).
     03  PRT-ST1           PIC X(04).
     03  DSP-ST            PIC X(02).
     03  DSP-ST1           PIC X(04).
*システム日付
 01  SYS-DATE.
     03  SYS-YY            PIC 9(02).
     03  SYS-MM            PIC 9(02).
     03  SYS-DD            PIC 9(02).
 01  WK-AREA.
     03  END-SW            PIC 9(01) VALUE     ZERO.
     03  ERR-SW            PIC 9(01) VALUE     ZERO.
     03  PAGE-CNT          PIC 9(05) VALUE     ZERO.
     03  LINE-CNT          PIC 9(05) VALUE     ZERO.
     03  WK-TORICD         PIC 9(08) VALUE     ZERO.
     03  WK-TENCD          PIC 9(05) VALUE     ZERO.
     03  WK-SYUKKA         PIC 9(02) VALUE     ZERO.
 01  IN-DATA               PIC X(01).
 01  FILE-ERR.
     03  DSP-ERR           PIC  N(10) VALUE
                        NC"画面ファイルエラー".
     03  TER-ERR           PIC N(12) VALUE
                        NC"店舗別ルートマスタエラー".
     03  TEN-ERR           PIC N(10) VALUE
                        NC"店舗マスタエラー".
     03  TOK-ERR           PIC N(10) VALUE
                        NC"取引先マスタエラー".
     03  SOK-ERR           PIC N(10) VALUE
                        NC"倉庫マスタエラー".
     03  PRT-ERR           PIC N(10) VALUE
                        NC"プリンターエラー".
 01  WK-ERR.
     03  WK-ERR1                  PIC  N(12) VALUE
              NC"　　終了が開始より小さい".
     03  WK-ERR2                  PIC  N(15) VALUE
              NC"　『店舗別ルートマスタ作成中』".
     03  WK-ERR4                  PIC  N(12) VALUE
              NC"　対象データがありません".
******************************************************************
*             PROCEDURE           DIVISION                       *
******************************************************************
 PROCEDURE                           DIVISION.
 DECLARATIVES.
 TER-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HTENRMS.
     DISPLAY     TER-ERR   UPON      STA.
     DISPLAY     TER-ST    UPON      STA.
     ACCEPT      IN-DATA   FROM      STA.
     STOP        RUN.
 TEN-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HTENMS.
     DISPLAY     TEN-ERR   UPON      STA.
     DISPLAY     TEN-ST    UPON      STA.
     ACCEPT      IN-DATA   FROM      STA.
     STOP        RUN.
 TOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HTOKMS.
     DISPLAY     TOK-ERR   UPON      STA.
     DISPLAY     TOK-ST    UPON      STA.
     ACCEPT      IN-DATA   FROM      STA.
     STOP        RUN.
 SOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE ZSOKMS.
     DISPLAY     SOK-ERR   UPON      STA.
     DISPLAY     SOK-ST    UPON      STA.
     ACCEPT      IN-DATA   FROM      STA.
     STOP        RUN.
 PRT-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE PRTF.
     DISPLAY     PRT-ERR   UPON      STA.
     DISPLAY     PRT-ST    UPON      STA.
     ACCEPT      IN-DATA   FROM      STA.
     STOP        RUN.
 DSP-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE DSPF.
     DISPLAY     DSP-ERR   UPON      STA.
     DISPLAY     DSP-ST    UPON      STA.
     ACCEPT      IN-DATA   FROM      STA.
     STOP        RUN.
 END DECLARATIVES.
***********************************************************
*                     ＭＡＩＮ処理                        *
***********************************************************
 PROC-SEC                  SECTION.
     OPEN        I-O       DSPF.
 PROC-010.
     PERFORM     INIT-SEC.
     PERFORM     MAIN-SEC  UNTIL     END-SW = 9  OR 1.
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
     ACCEPT      SYS-DATE  FROM      DATE.
     OPEN        INPUT     HTENRMS   HTENMS
                           HTOKMS    ZSOKMS
                 OUTPUT    PRTF.
************
*帳票初期化*
************
     MOVE        SPACE     TO        FVDA1731.
     INITIALIZE  FVDA1731.
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
****************************
*店舗別ルートマスタＲＥＡＤ*
****************************
 MAIN-010.
     READ   HTENRMS  NEXT  AT        END
         MOVE    1         TO        END-SW
         GO                TO        MAIN-EXIT.
*店舗チェック
**** IF  (TER-F03   <  KAITEN) OR (TER-F03   >  ENDTEN)
****     GO                TO        MAIN-010.
*出荷場所チェック
     IF  (TER-F02   <  KAISYU) OR (TER-F02   >  ENDSYU)
         GO                TO        MAIN-010.
*終了得意先チェック
     IF  (TER-F01  >  ENDTOK)
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
     CLOSE       PRTF      HTENRMS   HTENMS    HTOKMS  ZSOKMS.
 END-EXIT.
     EXIT.
**********************************************************
*                 見出しデータ編集書き出し               *
**********************************************************
 MIDA-SEC                  SECTION.
     MOVE        SYS-YY    TO        SYSYY.
     MOVE        SYS-MM    TO        SYSMM.
     MOVE        SYS-DD    TO        SYSDD.
     ADD         1         TO        PAGE-CNT.
     MOVE        ZERO      TO        WK-TORICD.
     MOVE        ZERO      TO        WK-TENCD.
     MOVE        ZERO      TO        WK-SYUKKA.
     MOVE        ZERO      TO        LINE-CNT.
     MOVE        PAGE-CNT  TO        LPAGE.
**************
*帳票書き出し*
**************
     MOVE       "FVDA1731" TO        PRT-FORM.
     MOVE       "GRP001"   TO        PRT-GRP.
     MOVE       "A000"     TO        PRT-CTL.
     MOVE       "PW"       TO        PRT-PROC.
     WRITE       FVDA1731.
     ADD         5         TO        LINE-CNT.
 MIDA-EXIT.
     EXIT.
**********************************************************
*                 明細情報の編集                         *
**********************************************************
 MEIEDT-SEC                  SECTION.
     IF    (WK-TORICD  =  TER-F01)
       AND (WK-SYUKKA  =  TER-F02)
          MOVE   ZERO      TO        TORICD
          MOVE   SPACE     TO        TORINM
**********MOVE   ZERO      TO        SYUKKA
          MOVE   SPACE     TO        SYUKKA
          MOVE   SPACE     TO        SYUNM
          GO               TO        MEIEDT-010
     ELSE
*        １行改行（取引先コードブレイク時）
         IF   LINE-CNT  NOT =     5
              MOVE     "A001"     TO        PRT-CTL
              MOVE     "PW"       TO        PRT-PROC
              MOVE     "GRP003"   TO        PRT-GRP
              MOVE     "FVDA1731" TO        PRT-FORM
              WRITE     FVDA1731
*             帳票初期化
              MOVE      SPACE     TO        FVDA1731
              INITIALIZE  FVDA1731
              ADD       1         TO        LINE-CNT
         END-IF
         MOVE   TER-F01   TO        TORICD  WK-TORICD
         MOVE   TER-F02   TO        SYUKKA  WK-SYUKKA
     END-IF.
*取引先ＲＥＡＤ
     MOVE        TER-F01   TO        TOK-F01.
     READ    HTOKMS
     INVALID
          MOVE   SPACE     TO        TORINM
     NOT INVALID
          MOVE   TOK-F02   TO        TORINM
     END-READ.
*倉庫マスタＲＥＡＤ
     MOVE        TER-F02   TO        SOK-F01.
     READ    ZSOKMS
     INVALID
          MOVE   SPACE     TO        SYUNM
     NOT INVALID
          MOVE   SOK-F02   TO        SYUNM
     END-READ.
*
 MEIEDT-010.
*
     IF    (WK-TENCD  =  TER-F03)
          MOVE   ZERO      TO        TENPCD
          MOVE   SPACE     TO        TNAME
          GO               TO        MEIEDT-020
     ELSE
          MOVE   TER-F03   TO        TENPCD  WK-TENCD
     END-IF.
*店舗マスタＲＥＡＤ
     MOVE        TER-F01   TO        TEN-F52.
     MOVE        TER-F03   TO        TEN-F011.
     READ    HTENMS
     INVALID
          MOVE   SPACE     TO        TNAME
     NOT INVALID
          MOVE   TEN-F02   TO        TNAME
     END-READ.
*
 MEIEDT-020.
     MOVE     TER-F04      TO        RUTO.
     MOVE     TER-F05      TO        RUTONO.
 MEIEDT-EXIT.
     EXIT.
**********************************************************
*                    データ書き出し                      *
**********************************************************
 MEIWRT-SEC                   SECTION.
**************
*帳票書き出し*
**************
     MOVE       "A001"     TO        PRT-CTL.
     MOVE       "PW"       TO        PRT-PROC.
     MOVE       "GRP002"   TO        PRT-GRP.
     MOVE       "FVDA1731" TO        PRT-FORM.
     WRITE       FVDA1731.
*帳票初期化*
     MOVE        SPACE     TO        FVDA1731.
     INITIALIZE  FVDA1731.
     ADD         1         TO        LINE-CNT.
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
**** IF  (ENDTEN = ZERO)
****     MOVE ALL "9"      TO        ENDTEN.
*****IF  (ENDSYU = ZERO)
     IF  (ENDSYU = SPACE)
         MOVE ALL "9"      TO        ENDSYU.
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
*出荷場所大小チェック
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
*店舗コード大小チェック
**** IF  (KAITEN > ENDTEN)
*        MOVE    1         TO        ERR-SW
*        MOVE   "R"        TO        EDIT-OPTION  OF    KAITEN
*        MOVE   "R"        TO        EDIT-OPTION  OF    ENDTEN
*        MOVE   "C"        TO        EDIT-CURSOR  OF    KAITEN
*        MOVE    WK-ERR1   TO        ERRMSG
*      ELSE
*        MOVE   "M"        TO        EDIT-OPTION  OF    KAITEN
*        MOVE   "M"        TO        EDIT-OPTION  OF    ENDTEN
****     MOVE    SPACE     TO        EDIT-CURSOR  OF    KAITEN.
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
****************************
*店舗別ルートマスタＲＥＡＤ*
****************************
 DSP-040.
     MOVE        KAITOK    TO        TER-F01.
     MOVE        KAISYU    TO        TER-F02.
     MOVE        ZERO      TO        TER-F03.
     MOVE        ZERO      TO        TER-F04.
     MOVE        ZERO      TO        TER-F05.
     START       HTENRMS   KEY  IS   >=     TER-F01
                                            TER-F02
                                            TER-F04
                                            TER-F05
                                            TER-F03
       INVALID
         MOVE    WK-ERR4   TO        ERRMSG
         MOVE   "ERRMSG"   TO        DSP-GRP
         PERFORM DSP-WRT-SEC
         MOVE    ZERO      TO        ERR-SW
         GO                TO        DSP-010.
 DSP-050.
     READ   HTENRMS  NEXT  AT        END
         MOVE    WK-ERR4   TO        ERRMSG
         MOVE   "ERRMSG"   TO        DSP-GRP
         PERFORM DSP-WRT-SEC
         MOVE    ZERO      TO        ERR-SW
         GO                TO        DSP-010.
*店舗チェック
**** IF  (TER-F03   <  KAITEN) OR (TER-F03   >  ENDTEN)
****     GO                TO        DSP-050.
*出荷場所チェック
     IF  (TER-F02   <  KAISYU) OR (TER-F02   >  ENDSYU)
         GO                TO        DSP-050.
*終了取引先チェック
     IF  (TER-F01  >  ENDTOK)
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
     WRITE       FVDA1730.
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
     MOVE        SPACE     TO        FVDA1730.
     MOVE       "SCREEN"   TO        DSP-GRP.
     MOVE       "FVDA1730" TO        DSP-FORM.
     MOVE       "CL"       TO        DSP-PROC.
     PERFORM     DSP-WRT-SEC.
     INITIALIZE  FVDA1730.
 DSP-INT-EXIT.
     EXIT.

```
