# SLST030

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SLST030.COB`

## ソースコード

```cobol
**********************************************************
*
*           商品名称マスタリスト
*           S L S T 0 3 0
*
* 変更履歴
*   93/10/27
*     範囲指定入力の商品コードを　
*     X(16) => X(08) X(05) X(02) X(01) に分割
*   00/07/11
*      定番区分、季節区分出力追加
*   2011/10/14  飯田/NAV  基幹サーバ統合・業務改善
*   2012/10/09  高橋/NAV  ＬＩＮＫＳ連携　物流束区分
*
**********************************************************
 IDENTIFICATION            DIVISION.
 PROGRAM-ID.               OLST030.
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
*商品名称マスタ
     SELECT     HMEIMS     ASSIGN    TO        DA-01-VI-MEIMS1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      DYNAMIC
                           RECORD    KEY       MEI-F011
                                               MEI-F0121
                                               MEI-F0122
                                               MEI-F0123
                           FILE      STATUS    MEI-ST.
*----<< 仕入先マスタ >>-*
     SELECT   ZSHIMS    ASSIGN    TO        DA-01-VI-ZSHIMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       SHI-F01.
*----<< 条件ファイル >>-*
     SELECT     HJYOKEN    ASSIGN    TO        DA-01-VI-JYOKEN1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       JYO-F01
                                               JYO-F02
                           FILE      STATUS    JYO-ST.
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
*商品名称マスタ
 FD  HMEIMS
     BLOCK       CONTAINS  24        RECORDS
     LABEL       RECORD    IS        STANDARD.
     COPY        HMEIMS    OF        XFDLIB
     JOINING     MEI       AS        PREFIX.
*条件ファイル
 FD  HJYOKEN.
     COPY        HJYOKEN   OF        XFDLIB
     JOINING     JYO       AS        PREFIX.
*仕入先マスタ
 FD  ZSHIMS.
     COPY        ZSHIMS    OF        XFDLIB
     JOINING     SHI       AS        PREFIX.
*プリンター
 FD  PRTF
     LABEL       RECORD    IS        OMITTED.
     COPY        FL030L    OF        XMDLIB.
*表示ファイル
 FD  DSPF
     LABEL       RECORD    IS        OMITTED.
     COPY        FL030     OF        XMDLIB.
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
     03  MEI-ST            PIC X(02).
     03  MEI-ST1           PIC X(04).
*
     03  JYO-ST            PIC X(02).
*
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
     03  WK-NYUKIN         PIC 9(02) VALUE     ZERO.
**93/10/27 S**
     03  HEN-FLG           PIC 9(01) VALUE     ZERO.
     03  I                 PIC 9(01) VALUE     ZERO.
     03  J                 PIC 9(01) VALUE     ZERO.
* 2011/10/14,S  S.I/NAV
     03  FG-HJYOKEN-INV     PIC  9(01).
* 2011/10/14,E  S.I/NAV
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
 01  IN-DATA               PIC X(01).
 01  FILE-ERR.
     03  DSP-ERR           PIC  N(10) VALUE
                        NC"画面ファイルエラー".
     03  MEI-ERR           PIC N(10) VALUE
                        NC"商品名称マスタエラー".
     03  PRT-ERR           PIC N(10) VALUE
                        NC"プリンターエラー".
     03  JYO-ERR           PIC N(10) VALUE
                        NC"条件ファイルエラー".
 01  WK-ERR.
     03  WK-ERR1                  PIC  N(12) VALUE
              NC"　　終了が開始より小さい".
     03  WK-ERR2                  PIC  N(12) VALUE
              NC"『商品名称マスタ作成中』".
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
 MEI-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HMEIMS.
     DISPLAY     MEI-ERR   UPON      STA.
     DISPLAY     MEI-ST    UPON      STA.
     ACCEPT      IN-DATA   FROM      STA.
*****MOVE        255       TO        PROGRAM-STATUS.
     STOP        RUN.
 PRT-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE PRTF.
     DISPLAY     PRT-ERR   UPON      STA.
     DISPLAY     PRT-ST    UPON      STA.
     ACCEPT      IN-DATA   FROM      STA.
*****MOVE        255       TO        PROGRAM-STATUS.
     STOP        RUN.
 DSP-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE DSPF.
     DISPLAY     DSP-ERR   UPON      STA.
     DISPLAY     DSP-ST    UPON      STA.
     ACCEPT      IN-DATA   FROM      STA.
*****MOVE        255       TO        PROGRAM-STATUS.
     STOP        RUN.
 JYO-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HJYOKEN.
     DISPLAY     JYO-ERR   UPON      STA.
     DISPLAY     JYO-ST    UPON      STA.
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
**** ACCEPT      SYS-DATE  FROM      DATE.
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
     OPEN        INPUT     HMEIMS  ZSHIMS  HJYOKEN.
     OPEN        OUTPUT    PRTF.
************
*帳票初期化*
************
     MOVE        SPACE     TO        FL030L  FL030.
     INITIALIZE  FL030L.
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
     IF  (LINE-CNT  >  60)
          PERFORM MIDA-SEC
     END-IF.
     PERFORM     MEIWRT-SEC.
**********************
*商品名称マスタＲＥＡＤ*
**********************
 MAIN-010.
     READ   HMEIMS   NEXT  AT        END
         MOVE    1         TO        END-SW
         GO                TO        MAIN-EXIT.
*終了商品コードチェック
     IF  (MEI-F01  >  WK-SYURYO)
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
     CLOSE       PRTF      HMEIMS     ZSHIMS     HJYOKEN.
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
     MOVE       "FL030L"   TO        PRT-FORM.
     MOVE       "GRP001"   TO        PRT-GRP.
     MOVE       "A000"     TO        PRT-CTL.
     MOVE       "PW"       TO        PRT-PROC.
     WRITE       FL030L.
     ADD         5         TO        LINE-CNT.
 MIDA-EXIT.
     EXIT.
**********************************************************
*                 明細情報の編集                         *
**********************************************************
 MEIEDT-SEC                  SECTION.
     MOVE        MEI-F01   TO        SHOCD.
     MOVE        MEI-F021  TO        SNAME1.
     MOVE        MEI-F022  TO        SNAME2.
     MOVE        MEI-F031  TO        SKANA1.
     MOVE        MEI-F032  TO        SKANA2.
     MOVE        MEI-F07   TO        IRESU.
     MOVE        MEI-F041  TO        GENKA.
     MOVE        MEI-F042  TO        BAIKA.
     MOVE        MEI-F043  TO        KOURI.
     MOVE        MEI-F05   TO        SIRCD.
     MOVE        MEI-F06   TO        JANCD.
     MOVE        MEI-F93    TO       KUBUN1.
     MOVE        MEI-F94    TO       KUBUN2.
     MOVE        MEI-F92    TO       HATKBN.
     MOVE        MEI-F95    TO       TEIBAN.
     MOVE        MEI-F96    TO       KISETU.
     MOVE        MEI-F05    TO       SHI-F01.
***           分類・管理区分追加
     MOVE        MEI-F90    TO       BUNRUI.
     MOVE        MEI-F91    TO       KANRI.
***09/08/31   分類名追加
****          条件ファイル索引
     IF  MEI-F90       NOT  =   SPACE
         MOVE          91            TO     JYO-F01
         MOVE          MEI-F90       TO     JYO-F02
         READ          HJYOKEN
              INVALID
                    MOVE     SPACE     TO        BUNMEI
                    CONTINUE
              NOT INVALID
                    MOVE     JYO-F03   TO        BUNMEI
         END-READ
      ELSE
         MOVE          SPACE         TO     BUNMEI
      END-IF.
*
*
*****DISPLAY  MEI-F05 UPON STA.
     READ        ZSHIMS
          INVALID
                 MOVE  SPACE TO      TOKNM
          NOT INVALID
                 MOVE  SHI-F02  TO   TOKNM
     END-READ.
*
* 2011/10/14,S  S.I/NAV
     MOVE  MEI-F09          TO  BNR20.  *> サカタ20分類

     MOVE  10               TO  JYO-F01.
     MOVE  MEI-F09          TO  JYO-F02.
     PERFORM  HJYOKEN-RD-SEC.
     IF  FG-HJYOKEN-INV = ZERO
         MOVE  JYO-F03      TO  BNR20N *> サカタ20分類名
     ELSE
         MOVE  SPACE        TO  BNR20N *> サカタ20分類名
     END-IF.

     MOVE  MEI-F10          TO  KOURKB. *> 小売連携区分

* 2011/10/14,E  S.I/NAV
***2012/10/09 物流束区分追加
***           物流束区分
     MOVE        MEI-F89    TO       BUTUR.
     EVALUATE    MEI-F89
         WHEN    SPACE  MOVE NC"束扱い無"  TO  BUTURN
         WHEN    "1"    MOVE NC"束扱い有"  TO  BUTURN
         WHEN    OTHER  MOVE NC"＊＊＊＊"  TO  BUTURN
     END-EVALUATE.
***2012/10/09 物流束区分追加

 MEIEDT-EXIT.
     EXIT.
**********************************************************
*    条件マスタ検索  2011/10/14 追加                     *
**********************************************************
 HJYOKEN-RD-SEC             SECTION.
     READ  HJYOKEN
       INVALID
         MOVE  1            TO  FG-HJYOKEN-INV
       NOT INVALID
         MOVE  ZERO         TO  FG-HJYOKEN-INV
     END-READ.

 HJYOKEN-RD-EXIT.
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
     MOVE       "FL030L"   TO        PRT-FORM.
     WRITE       FL030L.
*帳票初期化*
     MOVE        SPACE     TO        FL030L.
     INITIALIZE  FL030L.
     ADD         4         TO        LINE-CNT.
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
     IF  (ENDSHO = SPACE)
         MOVE ALL "9"      TO        ENDSHO
     END-IF.
     IF  (ENDSH5 = SPACE)
         MOVE ALL "9"      TO        ENDSH5
     END-IF.
     IF  (ENDSH2 = SPACE)
         MOVE ALL "9"      TO        ENDSH2
     END-IF.
     IF  (ENDSH1 = SPACE)
         MOVE ALL "9"      TO        ENDSH1
     END-IF.
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
     MOVE   KAISHO     TO        WK-KAISHO.
     MOVE   KAISH5     TO        WK-KAISH5.
     MOVE   KAISH2     TO        WK-KAISH2.
     MOVE   KAISH1     TO        WK-KAISH1.
     MOVE   ENDSHO     TO        WK-ENDSHO.
     MOVE   ENDSH5     TO        WK-ENDSH5.
     MOVE   ENDSH2     TO        WK-ENDSH2.
     MOVE   ENDSH1     TO        WK-ENDSH1.
*商品コード大小チェック
     IF  (WK-KAISHI > WK-SYURYO)
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
*商品名称マスタＲＥＡＤ*
**********************
 DSP-040.
     MOVE        WK-KAISHI TO        MEI-F01.
     START       HMEIMS    KEY  IS   >=
                 MEI-F011  MEI-F0121  MEI-F0122  MEI-F0123
       INVALID
         MOVE    WK-ERR4   TO        ERRMSG
         MOVE   "ERRMSG"   TO        DSP-GRP
         PERFORM DSP-WRT-SEC
         MOVE    ZERO      TO        ERR-SW
         GO                TO        DSP-010.
 DSP-050.
     READ   HMEIMS   NEXT  AT        END
         MOVE    WK-ERR4   TO        ERRMSG
         MOVE   "ERRMSG"   TO        DSP-GRP
         PERFORM DSP-WRT-SEC
         MOVE    ZERO      TO        ERR-SW
         GO                TO        DSP-010.
*終了商品チェック
     IF  (MEI-F01  >  WK-SYURYO)
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
     WRITE       FL030.
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
     MOVE        SPACE     TO        FL030.
     MOVE       "SCREEN"   TO        DSP-GRP.
     MOVE       "FL030"    TO        DSP-FORM.
     MOVE        SPACE     TO        DSP-PROC.
     MOVE        HEN-DATE  TO        SDATE.
     MOVE        HEN-TIME  TO        STIME.
     PERFORM     DSP-WRT-SEC.
 DSP-INT-EXIT.
     EXIT.

```
