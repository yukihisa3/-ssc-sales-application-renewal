# SIT0100L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SIT0100L.COB`

## ソースコード

```cobol
**********************************************************
*
*         　商品名称マスタリスト（ＩＴ統制対応）
*           SIT0100L(SLST030改修)
*
* 更新履歴
*   93/10/27
*     範囲指定入力の商品コードを　
*     X(16) => X(08) X(05) X(02) X(01) に分割
*   00/07/11
*     定番区分、季節区分出力追加
*   2011/10/14 飯田/NAV 基幹サーバ統合・業務改善
*
**********************************************************
 IDENTIFICATION            DIVISION.
 PROGRAM-ID.               SIT0100L.
 AUTHOR.                   S.I.
 DATE-WRITTEN.             09/03/18.
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
*マスタ更新履歴ファイル
     SELECT     MSTLOGF    ASSIGN    TO        MSTLOGL3
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      SEQUENTIAL
                           RECORD    KEY       MSL-F01
                                               MSL-F02
                                               MSL-F03
                                               MSL-F05
                                               MSL-F06
                                               MSL-F07
                           FILE      STATUS    MSL-ST.
*----<< 仕入先マスタ >>-*
     SELECT     ZSHIMS    ASSIGN     TO        DA-01-VI-ZSHIMS1
                          ORGANIZATION         INDEXED
                          ACCESS     MODE      RANDOM
                          RECORD     KEY       SHI-F01.
*担当者マスタ
     SELECT     HTANMS     ASSIGN    TO        TANMS1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       TAN-F01
                                               TAN-F02
                           FILE      STATUS    TAN-ST.

* 2011/10/14,S  S.I/NAV
*---<<  条件ファイル  >>---*
     SELECT   HJYOKEN   ASSIGN    TO        DA-01-VI-JYOKEN1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   JYO-F01
                                                 JYO-F02
                        FILE      STATUS    IS   JYO-ST.
* 2011/10/14,E  S.I/NAV

*%* プリンター *%*
     SELECT     PRTF       ASSIGN    TO        GS-PRTF
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
*マスタ更新履歴ファイル
 FD  MSTLOGF
     BLOCK       CONTAINS   8        RECORDS
     LABEL       RECORD    IS        STANDARD.
     COPY        MSTLOGF   OF        XFDLIB
     JOINING     MSL       AS        PREFIX.
*担当者マスタ
 FD  HTANMS
     BLOCK       CONTAINS   8        RECORDS
     LABEL       RECORD    IS        STANDARD.
     COPY        HTANMS    OF        XFDLIB
     JOINING     TAN       AS        PREFIX.
 FD  ZSHIMS.
     COPY        ZSHIMS    OF        XFDLIB
     JOINING     SHI       AS        PREFIX.

* 2011/10/14,S  S.I/NAV
*---<<  条件ファイル  >>---*
 FD  HJYOKEN.
     COPY     HJYOKEN   OF        XFDLIB
              JOINING   JYO       PREFIX.
* 2011/10/14,E  S.I/NAV

*プリンター
 FD  PRTF
     LABEL       RECORD    IS        OMITTED.
     COPY        FIT01001  OF        XMDLIB.
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
     03  MSL-ST            PIC X(02).
     03  MSL-ST1           PIC X(04).
     03  TAN-ST            PIC X(02).
     03  TAN-ST1           PIC X(04).
     03  PRT-ST            PIC X(02).
     03  PRT-ST1           PIC X(04).
* 2011/10/14,S  S.I/NAV
     03  JYO-ST            PIC X(02).
* 2011/10/14,E  S.I/NAV
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
     03  MSL-ERR           PIC  N(11) VALUE
                        NC"マスタ更新履歴Ｆエラー".
     03  TAN-ERR           PIC N(10) VALUE
                        NC"担当者マスタエラー".
     03  PRT-ERR           PIC N(10) VALUE
                        NC"プリンターエラー".
* 2011/10/14,S  S.I/NAV
     03  JYO-ERR           PIC N(10) VALUE
                        NC"条件マスタエラー".
* 2011/10/14,E  S.I/NAV
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
*帳票　更新履歴編集
 01  HEN-KUBUN.
     03  FILLER            PIC  N(01)  VALUE NC"［".
     03  HEN-KUBUNNM       PIC  N(02).
     03  FILLER            PIC  N(01)  VALUE NC"：".
 01  HEN-TAN.
     03  HEN-TANNM         PIC  N(10).
     03  FILLER            PIC  N(01)  VALUE NC"：".
*担当者コード
 01  WK-TANCD.
     03  WK-TANCD1         PIC  X(02).
     03  WK-FILLER         PIC  X(06).
*マスタレコードエリア（店舗マスタ）
     COPY    HMEIMS        OF   XFDLIB
     JOINING MEI           AS   PREFIX.
*更新範囲
 01  TRND-DT.
     03  TRND-DATE         PIC  9(08).
     03  TRND-TIME         PIC  9(06).
 01  TO-DT.
     03  TO-DATE           PIC  9(08).
     03  TO-TIME           PIC  9(06).
*
 01  READ-CNT              PIC  9(07) VALUE 0.
 01  IN-CNT                PIC  9(07) VALUE 0.
*
 LINKAGE                   SECTION.
 01  PARA-BUMONCD          PIC X(04).
 01  PARA-TANCD            PIC X(08).
 01  PARA-UPDTDATE         PIC 9(08).
 01  PARA-UPDTIME          PIC 9(06).
 01  PARA-UPDTDATE-E       PIC 9(08).
 01  PARA-UPDTIME-E        PIC 9(06).
******************************************************************
*             PROCEDURE           DIVISION                       *
******************************************************************
 PROCEDURE                 DIVISION USING PARA-BUMONCD
                                          PARA-TANCD
                                          PARA-UPDTDATE
                                          PARA-UPDTIME
                                          PARA-UPDTDATE-E
                                          PARA-UPDTIME-E.
 DECLARATIVES.
 MSL-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE MSTLOGF.
     DISPLAY     MSL-ERR   UPON      STA.
     DISPLAY     MSL-ST    UPON      STA.
     ACCEPT      IN-DATA   FROM      STA.
*****MOVE        255       TO        PROGRAM-STATUS.
     STOP        RUN.
 TAN-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HTANMS.
     DISPLAY     TAN-ERR   UPON      STA.
     DISPLAY     TAN-ST    UPON      STA.
     ACCEPT      IN-DATA   FROM      STA.
*****MOVE        255       TO        PROGRAM-STATUS.
     STOP        RUN.

* 2011/10/14,S  S.I/NAV
 JYO-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HJYOKEN.
     DISPLAY     JYO-ERR   UPON      STA.
     DISPLAY     JYO-ST    UPON      STA.
     ACCEPT      IN-DATA   FROM      STA.
*****MOVE        255       TO        PROGRAM-STATUS.
     STOP        RUN.
* 2011/10/14,E  S.I/NAV

 PRT-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE PRTF.
     DISPLAY     PRT-ERR   UPON      STA.
     DISPLAY     PRT-ST    UPON      STA.
     ACCEPT      IN-DATA   FROM      STA.
*****MOVE        255       TO        PROGRAM-STATUS.
     STOP        RUN.
 END DECLARATIVES.
***********************************************************
*                     ＭＡＩＮ処理                        *
***********************************************************
 PROC-SEC                  SECTION.
*
 PROC-010.
     PERFORM     INIT-SEC.
     PERFORM     MAIN-SEC  UNTIL     END-SW = 9  OR 1
     PERFORM     END-SEC.
     STOP        RUN.
 PROC-EXIT.
     EXIT.
**********************************************************
*                      Ｉ Ｎ Ｉ Ｔ                       *
**********************************************************
 INIT-SEC                  SECTION.
     MOVE        ZERO      TO        PAGE-CNT.
     MOVE        61        TO        LINE-CNT.
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
*パラメタ：担当者コード取得
     MOVE     PARA-TANCD          TO   WK-TANCD.
*
     OPEN        INPUT     MSTLOGF  HTANMS  ZSHIMS
                 OUTPUT    PRTF.
* 2011/10/14,S  S.I/NAV
     OPEN  INPUT HJYOKEN.
* 2011/10/14,E  S.I/NAV
************
*帳票初期化*
************
     MOVE        SPACE     TO        FIT01001.
     INITIALIZE  FIT01001.
*マスタ更新履歴ファイル初期ＲＥＡＤ
     MOVE     "04"                TO   MSL-F01.
     MOVE     PARA-BUMONCD        TO   MSL-F02
     MOVE     WK-TANCD            TO   MSL-F03.
     MOVE     PARA-UPDTDATE       TO   MSL-F05.
     MOVE     PARA-UPDTIME        TO   MSL-F06.
     START    MSTLOGF            KEY IS >= MSL-F01
                                           MSL-F02
                                           MSL-F03
                                           MSL-F05
                                           MSL-F06
              INVALID  MOVE  9    TO   END-SW
                       GO TO      INIT-EXIT.
     PERFORM  READMSL-SEC.
*
 INIT-EXIT.
     EXIT.
***********************************************************
*                     ＭＡＩＮ処理                        *
***********************************************************
 MAIN-SEC                  SECTION.
*
     PERFORM     MEIEDT-SEC.
*
     IF  (LINE-CNT  >  60)
          PERFORM MIDA-SEC
     END-IF.
*
     PERFORM     MEIWRT-SEC.
     PERFORM     READMSL-SEC.
*
 MAIN-EXIT.
     EXIT.
**********************
*マスタ更新履歴ファイルＲＥＡＤ*
**********************
 READMSL-SEC               SECTION.
*
     READ   MSTLOGF        AT        END
         MOVE    9         TO        END-SW
         GO                TO        READMSL-EXIT.
*
     MOVE   MSL-F05        TO        TRND-DATE.
     MOVE   MSL-F06        TO        TRND-TIME.
     MOVE   PARA-UPDTDATE-E TO       TO-DATE.
     MOVE   PARA-UPDTIME-E  TO       TO-TIME.
*
     IF  (MSL-F01  >  "04") OR (MSL-F02  >  PARA-BUMONCD) OR
         (MSL-F03  >  PARA-TANCD) OR (TRND-DT > TO-DT)
         MOVE    9         TO        END-SW
         GO      TO        READMSL-EXIT.
     ADD         1         TO        READ-CNT.
*
 READMSL-EXIT.
     EXIT.
**********************************************************
*                       Ｅ Ｎ Ｄ                         *
**********************************************************
 END-SEC                   SECTION.
*
     CLOSE       PRTF      MSTLOGF  HTANMS  ZSHIMS.
* 2011/10/14,S  S.I/NAV
     CLOSE  HJYOKEN.
* 2011/10/14,E  S.I/NAV
     DISPLAY  "SIT0100L READ =" READ-CNT UPON STA.
     DISPLAY  "SIT0100L IN   =" IN-CNT   UPON STA.
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
     MOVE       "FIT01001" TO        PRT-FORM.
     MOVE       "GRP001"   TO        PRT-GRP.
     MOVE       "A000"     TO        PRT-CTL.
     MOVE       "PW"       TO        PRT-PROC.
     WRITE       FIT01001.
     ADD         9         TO        LINE-CNT.
 MIDA-EXIT.
     EXIT.
**********************************************************
*                 明細情報の編集                         *
**********************************************************
 MEIEDT-SEC                  SECTION.
     ADD         1         TO        IN-CNT.
     MOVE        MSL-F08   TO        MEI-REC.
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
     MOVE        MEI-F93   TO        KUBUN1.
     MOVE        MEI-F94   TO        KUBUN2.
     MOVE        MEI-F92   TO        HATKBN.
     MOVE        MEI-F95   TO        TEIBAN.
     MOVE        MEI-F96   TO        KISETU.
     MOVE        MEI-F05   TO        SHI-F01.
*****DISPLAY  MEI-F05 UPON STA.
     READ        ZSHIMS
          INVALID
                 MOVE  SPACE TO      TOKNM
          NOT INVALID
                 MOVE  SHI-F02  TO   TOKNM
     END-READ.
*
*更新履歴
     EVALUATE    MSL-F04
         WHEN    "1"
                 MOVE NC"登録"          TO   HEN-KUBUNNM
         WHEN    "2"
                 MOVE NC"修正"          TO   HEN-KUBUNNM
         WHEN    "3"
                 MOVE NC"削除"          TO   HEN-KUBUNNM
     END-EVALUATE.
     MOVE        HEN-KUBUN              TO   UDKBN.
*
     MOVE        MSL-F03                TO   WK-TANCD.
     MOVE        WK-TANCD1              TO   TANCD.
*担当者名取得
     MOVE        MSL-F02                TO   TAN-F01.
     MOVE        WK-TANCD1              TO   TAN-F02.
     READ        HTANMS
       INVALID
                 MOVE   SPACE           TO  HEN-TANNM
       NOT INVALID
                 MOVE   TAN-F03         TO  HEN-TANNM
     END-READ.
*
     MOVE        HEN-TAN                TO  TANNM.
     MOVE        MSL-F05(1:4)           TO  HEN-DATE-YYYY.
     MOVE        MSL-F05(5:2)           TO  HEN-DATE-MM.
     MOVE        MSL-F05(7:2)           TO  HEN-DATE-DD.
     MOVE        HEN-DATE               TO  UPDT.
*
     MOVE        MSL-F06(1:2)           TO  HEN-TIME-HH.
     MOVE        MSL-F06(3:2)           TO  HEN-TIME-MM.
     MOVE        MSL-F06(5:2)           TO  HEN-TIME-SS.
     MOVE        HEN-TIME               TO  UPTM.
*
     MOVE        NC"］"                 TO  KAKO.

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
*
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
     MOVE       "FIT01001" TO        PRT-FORM.
     WRITE       FIT01001.
*帳票初期化*
     MOVE        SPACE     TO        FIT01001.
     INITIALIZE  FIT01001.
     ADD         4         TO        LINE-CNT.
 MEIWRT-EXIT.
     EXIT.

```
