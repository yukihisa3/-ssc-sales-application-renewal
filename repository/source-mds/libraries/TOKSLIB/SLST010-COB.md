# SLST010

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SLST010.COB`

## ソースコード

```cobol
****************************************************************
*
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　
*    サブシステム　　　　：　　　　　　
*    業務名　　　　　　　：　取引先マスタ　　　　　　　
*    モジュール名　　　　：　取引先マスタリスト
*    作成日／作成者　　　：　92/11/10  /Y.Y
*    処理概要　　　　　　：　　　　　　　　　　　　　　　　　　
*    更新履歴　　　　　　：
*      1999/09/27  /HAGIWARA
*        新郵便番号追加
*        代表倉庫（倉庫マスタＲＥＡＤ）追加
*        自社指定伝票タイプ追加
*
*      2011/10/13 飯田/NAV 基幹サーバ統合・業務改善
*      2011/12/07 三浦/NAV シーズン開始日追加
*      2012/10/31 武井/NAV 流通ＢＭＳ対応
*      2013/12/11 井上/NAV 消費税増税対応
*      2013/12/24 井上/NAV 消費税　不要項目カット
*      2013/12/25 井上/NAV 消費税　項目追加
****************************************************************
 IDENTIFICATION            DIVISION.
 PROGRAM-ID.               SLST010.
 AUTHOR.                   Y.Y.
 DATE-WRITTEN.             92/11/10.
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
*取引先マスタ
     SELECT     HTOKMS     ASSIGN    TO        TOKMS2
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      DYNAMIC
                           RECORD    KEY       TOK-F01
                           FILE      STATUS    TOK-ST.
*倉庫マスタ
     SELECT     ZSOKMS     ASSIGN    TO        ZSOKMS1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       SOK-F01
                           FILE      STATUS    SOK-ST.
*2013/12/11↓
*---<<  条件ファイル  >>---*
     SELECT   HJYOKEN   ASSIGN    TO        DA-01-VI-JYOKEN1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   JYO-F01
                                                 JYO-F02
                        FILE      STATUS    IS   JYO-ST.
*2013/12/11↑
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
*取引先マスタ
 FD  HTOKMS
     BLOCK       CONTAINS   8        RECORDS
     LABEL       RECORD    IS        STANDARD.
     COPY        HTOKMS    OF        XFDLIB
     JOINING     TOK       AS        PREFIX.
*倉庫マスタ
 FD  ZSOKMS
     BLOCK       CONTAINS   8        RECORDS
     LABEL       RECORD    IS        STANDARD.
     COPY        ZSOKMS    OF        XFDLIB
     JOINING     SOK       AS        PREFIX.
*2013/12/11↓
*---<<  条件ファイル  >>---*
 FD  HJYOKEN.
     COPY     HJYOKEN   OF        XFDLIB
              JOINING   JYO       PREFIX.
*2013/12/11↑
*プリンター
 FD  PRTF
     LABEL       RECORD    IS        OMITTED.
     COPY        FL010L    OF        XMDLIB.
*表示ファイル
 FD  DSPF
     LABEL       RECORD    IS        OMITTED.
     COPY        FL010     OF        XMDLIB.
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
     03  TOK-ST            PIC X(02).
     03  TOK-ST1           PIC X(04).
     03  PRT-ST            PIC X(02).
     03  PRT-ST1           PIC X(04).
     03  DSP-ST            PIC X(02).
     03  DSP-ST1           PIC X(04).
     03  SOK-ST            PIC X(02).
*2013/12/11↓
     03  JYO-ST            PIC X(02).
*2013/12/11↑
*システム日付
*01  SYS-DATE              PIC X(06).
*01  SYS-DATE-R      REDEFINES  SYS-DATE.
*    03  WK-YYYY           PIC  9(04).
*    03  WK-MM             PIC  9(02).
*    03  WK-DD             PIC  9(02).
 01  WK-AREA.
     03  END-SW            PIC 9(01) VALUE     ZERO.
     03  ERR-SW            PIC 9(01) VALUE     ZERO.
     03  PAGE-CNT          PIC 9(05) VALUE     ZERO.
     03  LINE-CNT          PIC 9(05) VALUE     ZERO.
     03  WK-NYUKIN         PIC 9(02) VALUE     ZERO.
*2013/12/11↓
 01  JYO-INVALID-FLG       PIC  9(01) VALUE ZERO.
 01  WK-TOK-F19            PIC  9(11) VALUE ZERO.
 01  WK-TOK-F20            PIC  9(11) VALUE ZERO.
 01  WK-TOK-F21            PIC  9(11) VALUE ZERO.
*2013/12/11↑
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
 01  IN-DATA               PIC X(01).
 01  FILE-ERR.
     03  DSP-ERR           PIC  N(10) VALUE
                        NC"画面ファイルエラー".
     03  TOK-ERR           PIC N(10) VALUE
                        NC"取引先マスタエラー".
     03  PRT-ERR           PIC N(10) VALUE
                        NC"プリンターエラー".
     03  SOK-ERR           PIC N(10) VALUE
                        NC"倉庫マスタエラー".
*2013/12/11↓
     03  JYO-ERR           PIC N(10) VALUE
                        NC"条件ファイルエラー".
*2013/12/11↑
 01  WK-ERR.
     03  WK-ERR1                  PIC  N(12) VALUE
              NC"　　終了が開始より小さい".
     03  WK-ERR2                  PIC  N(12) VALUE
              NC"　『取引先マスタ作成中』".
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
 TOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HTOKMS.
     DISPLAY     TOK-ERR   UPON      STA.
     DISPLAY     TOK-ST    UPON      STA.
     ACCEPT      IN-DATA   FROM      STA.
     MOVE        4000      TO        PROGRAM-STATUS.
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
     MOVE        4000      TO        PROGRAM-STATUS.
     STOP        RUN.
 SOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE ZSOKMS.
     DISPLAY     SOK-ERR   UPON      STA.
     DISPLAY     SOK-ST    UPON      STA.
     ACCEPT      IN-DATA   FROM      STA.
     MOVE        4000      TO        PROGRAM-STATUS.
     STOP        RUN.
*2013/12/11↓
 JYO-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HJYOKEN.
     DISPLAY     JYO-ERR   UPON      STA.
     DISPLAY     JYO-ST    UPON      STA.
     ACCEPT      IN-DATA   FROM      STA.
     MOVE        4000      TO        PROGRAM-STATUS.
     STOP        RUN.
*2013/12/11↑
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
*
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
     OPEN        INPUT     HTOKMS  ZSOKMS
                 OUTPUT    PRTF.
*2013/12/11↓
     OPEN        INPUT     HJYOKEN.
*2013/12/11↑
************
*帳票初期化*
************
     MOVE        SPACE     TO        FL010L.
     INITIALIZE  FL010L.
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
     PERFORM     MEIEDT-SEC.
*2013/12/11↓
**** IF  (LINE-CNT  >  60)
     IF  (LINE-CNT  >  55)
          PERFORM MIDA-SEC
     END-IF.
*2013/12/11↑
     PERFORM     MEIWRT-SEC.
**********************
*取引先マスタＲＥＡＤ*
**********************
 MAIN-010.
     READ   HTOKMS   NEXT  AT        END
         MOVE    1         TO        END-SW
         GO                TO        MAIN-EXIT.
*終了得意先チェック
     IF  (TOK-F01  >  ENDTOK)
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
     CLOSE       PRTF      HTOKMS    ZSOKMS.
*2013/12/11↓
     CLOSE       HJYOKEN.
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
     MOVE        PAGE-CNT  TO        LPAGE.
**************
*帳票書き出し*
**************
     MOVE       "FL010L"   TO        PRT-FORM.
     MOVE       "GRP001"   TO        PRT-GRP.
     MOVE       "A000"     TO        PRT-CTL.
     MOVE       "PW"       TO        PRT-PROC.
     WRITE       FL010L.
*2013/12/11↓
* 2011/10/13,S  飯田/NAV
***** ADD         6         TO        LINE-CNT.
*12/11ADD         7         TO        LINE-CNT.
      ADD         8         TO        LINE-CNT.
* 2011/10/13,E  飯田/NAV
*2013/12/11↑
 MIDA-EXIT.
     EXIT.
**********************************************************
*                 明細情報の編集                         *
**********************************************************
 MEIEDT-SEC                  SECTION.
     MOVE        TOK-F01   TO        TORICD.
     MOVE        TOK-F02   TO        TNAME.
     MOVE        TOK-F04   TO        TKANA.
     MOVE        TOK-F801  TO        NYBN1.
     MOVE        TOK-F802  TO        NYBN2.
     MOVE        TOK-F05   TO        YUBIN.
     MOVE        TOK-F06   TO        JYU1.
     MOVE        TOK-F07   TO        JYU2.
     MOVE        TOK-F08   TO        TEL.
     MOVE        TOK-F09   TO        FAX.
     MOVE        TOK-F52   TO        TOKUCD.
     MOVE        TOK-F54   TO        DENNO.
     MOVE        TOK-F12(1) TO       SIME1.
     MOVE        TOK-F12(2) TO       SIME2.
     MOVE        TOK-F12(3) TO       SIME3.
     MOVE        TOK-F55   TO        GENKAR.
     MOVE        TOK-F56   TO        BAIKAR.
     MOVE        TOK-F84   TO        KUBUN1.
     MOVE        TOK-F88   TO        KUBUN2.
     MOVE        TOK-F91   TO        KUBUN3.
     MOVE        TOK-F92   TO        KUBUN4.
     MOVE        TOK-F93   TO        KUBUN5.
***09/08/28   管理区分追加
     MOVE        TOK-F78   TO        KANRI.
* 2011/10/13,S  S.I/NAV
     MOVE  TOK-FIL1(2:1)  TO  HCSYU. *>発注集計表出力区分
* 2011/10/13,S  S.I/NAV
* 2011/12/07,S  S.I/NAV
     MOVE  TOK-F70         TO  HARU.
     MOVE  TOK-F71         TO  AKI.
* 2011/12/07,S  S.I/NAV
     MOVE        TOK-F81   TO        SOKOCD  SOK-F01.
***  倉庫名称取得
     READ        ZSOKMS
       INVALID
                 MOVE   SPACE       TO  SOKONM
       NOT INVALID
                 MOVE  SOK-F02      TO  SOKONM
     END-READ.
*## 1999/11/02 NAV T.T START ##*
     MOVE        TOK-F79   TO        DENCD.
     EVALUATE    DENCD
         WHEN    0
                 MOVE NC"指定無し　　"  TO   DENCNM
         WHEN    1
                 MOVE NC"ＴＡ_型　　"  TO   DENCNM
         WHEN    2
                 MOVE NC"ＴＡ_型　　"  TO   DENCNM
         WHEN    3
                 MOVE NC"タイプ用　　"  TO   DENCNM
         WHEN    4
                 MOVE NC"タイプ用_型"  TO   DENCNM
     END-EVALUATE.
* 2012/10/31
     MOVE        TOK-F95   TO        STOKBN.
*
*2013/12/11↓
*<税扱い区分>
 ZEIA.
     EVALUATE    TOK-F18
          WHEN   1
                 MOVE NC"内税"          TO  ZEIA
          WHEN   2
                 MOVE NC"外税"          TO  ZEIA
          WHEN   OTHER
                 MOVE NC"？？"          TO  ZEIA
     END-EVALUATE.
*2013/12/24↓
*<計上税区分>
*ZEIKB1.
*    MOVE        50                     TO  JYO-F01.
*    MOVE        TOK-F19                TO  WK-TOK-F19.
*    MOVE        WK-TOK-F19(11:1)       TO  JYO-F02.
*    PERFORM     HJYOKEN-READ-SUB.
*    IF          JYO-INVALID-FLG   =    1
*                MOVE  NC"？？？？？"   TO  ZEIKB1
*    ELSE
*                MOVE  JYO-F03          TO  ZEIKB1
*    END-IF.
*2013/12/24↑
*2013/12/24↓
*<税区分改訂日>
*CHGDAY.
*    MOVE        TOK-F20                TO  WK-TOK-F20.
*    MOVE        WK-TOK-F20(4:8)        TO  CHGDAY.
*2013/12/24↑
*2013/12/24↓
*<改定後計上税区分>
*ZEIKB2.
*    IF          TOK-F20      =         ZERO
*                MOVE  SPACE  TO        ZEIKB2
*                GO           TO        ZEIN
*    END-IF.
*    MOVE        50                     TO  JYO-F01.
*    MOVE        TOK-F21                TO  WK-TOK-F21.
*    MOVE        WK-TOK-F21(11:1)       TO  JYO-F02.
*    PERFORM     HJYOKEN-READ-SUB.
*    IF          JYO-INVALID-FLG   =    1
*                MOVE  NC"？？？？？"   TO  ZEIKB2
*    ELSE
*                MOVE  JYO-F03          TO  ZEIKB2
*    END-IF.
*2013/12/24↑
*<税抜き計算区分>
 ZEIN.
*    MOVE        " )"                   TO  KAKKO2.
*2013/12/25↓
*    EVALUATE    TOK-F22
     EVALUATE    TOK-F20
*2013/12/25↑
          WHEN   0
                 MOVE NC"しない"        TO  ZEIN
          WHEN   1
                 MOVE NC"する　"        TO  ZEIN
          WHEN   OTHER
                 MOVE NC"？？？？？"    TO  ZEIN
     END-EVALUATE.
*2013/12/11↑
*2013/12/25↓
*<税抜き計算区分改訂日>
 CHGDAY.
     MOVE        TOK-F21                TO  WK-TOK-F21.
     MOVE        WK-TOK-F21(4:8)        TO  CHGDAY.
*<改定後税抜き計算区分>
 ZEIN2.
     EVALUATE    TOK-F22
          WHEN   0
                 MOVE NC"しない"        TO  ZEIN2
          WHEN   1
                 MOVE NC"する　"        TO  ZEIN2
          WHEN   OTHER
                 MOVE NC"？？？？？"    TO  ZEIN2
     END-EVALUATE.
     IF   TOK-F21      =         ZERO
          MOVE  SPACE  TO        ZEIN2
     END-IF.
*2013/12/25↑
*
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
     MOVE       "FL010L"   TO        PRT-FORM.
     WRITE       FL010L.
*帳票初期化*
     MOVE        SPACE     TO        FL010L.
     INITIALIZE  FL010L.
*2013/12/11↓
* 2011/10/13,S  飯田/NAV
****** ADD         3         TO        LINE-CNT.
*12/11ADD         4         TO        LINE-CNT.
      ADD         5         TO        LINE-CNT.
* 2011/10/13,E  飯田/NAV
*2013/12/11↑
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
*得意先コード大小チェック
     IF  (ENDTOK = ZERO)
         MOVE ALL "9"      TO        ENDTOK.
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
     MOVE        KAITOK    TO        TOK-F01
     START       HTOKMS    KEY  IS   >=   TOK-F01
       INVALID
         MOVE    WK-ERR4   TO        ERRMSG
         MOVE   "ERRMSG"   TO        DSP-GRP
         PERFORM DSP-WRT-SEC
         MOVE    ZERO      TO        ERR-SW
         GO                TO        DSP-010.
 DSP-050.
     READ   HTOKMS   NEXT  AT        END
         MOVE    WK-ERR4   TO        ERRMSG
         MOVE   "ERRMSG"   TO        DSP-GRP
         PERFORM DSP-WRT-SEC
         MOVE    ZERO      TO        ERR-SW
         GO                TO        DSP-010.
*終了請求先チェック
     IF  (TOK-F01  >  ENDTOK)
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
 DSP-EXIT.
     EXIT.
***********************************************************
*                       画面出力                          *
***********************************************************
 DSP-WRT-SEC               SECTION.
     MOVE        SPACE     TO        DSP-PROC.
     WRITE       FL010.
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
     MOVE        SPACE     TO        FL010.
     MOVE       "SCREEN"   TO        DSP-GRP.
     MOVE       "FL010"    TO        DSP-FORM.
     MOVE       "CL"       TO        DSP-PROC.
     MOVE        HEN-DATE  TO        SDATE.
     MOVE        HEN-TIME  TO        STIME.
     PERFORM     DSP-WRT-SEC.
     INITIALIZE  FL010.
 DSP-INT-EXIT.
     EXIT.
****************************************************************
*      3.0        条件ファイル索引                             *
****************************************************************
 HJYOKEN-READ-SUB       SECTION.
     MOVE     0                  TO   JYO-INVALID-FLG.
     READ     HJYOKEN
        INVALID
              MOVE      1        TO   JYO-INVALID-FLG
     END-READ.
 HJYOKEN-READ-END.
     EXIT.

```
