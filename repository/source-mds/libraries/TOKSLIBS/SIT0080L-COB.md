# SIT0080L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SIT0080L.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　　　　　　                        *
*    業務名　　　　　　　：　店舗マスタ　　　　　　　　        *
*    モジュール名　　　　：　店舗マスタリスト　                *
*    作成日／作成者　　　：　92/11/10  /Y.Y                    *
*    更新日／更新者　　　：　99/09/28  /HAGIWARA               *
*            新郵便番号追加                                    *
*            日付８桁変換ＰＧ（ＳＫＹＤＴＣＫＢ）使用          *
*    更新日／更新者　　　：　09/03/17  /IMAI                   *
*            ＩＴ統制対応                                      *
*    処理概要　　　　　　：　　　　　　　　　　　　　　　　　　*
*                                                              *
****************************************************************
 IDENTIFICATION            DIVISION.
 PROGRAM-ID.               SIT0080L.
 AUTHOR.                   S.I.
 DATE-WRITTEN.             09/03/17.
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
     SELECT     HTOKMS     ASSIGN    TO        DA-01-VI-TOKMS2
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       TOK-F01
                           FILE      STATUS    TOK-ST.
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
*担当者マスタ
     SELECT     HTANMS     ASSIGN    TO        TANMS1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       TAN-F01
                                               TAN-F02
                           FILE      STATUS    TAN-ST.
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
*取引先マスタ
 FD  HTOKMS
     BLOCK       CONTAINS   8        RECORDS
     LABEL       RECORD    IS        STANDARD.
     COPY        HTOKMS    OF        XFDLIB
     JOINING     TOK       AS        PREFIX.
*プリンター
 FD  PRTF
     LABEL       RECORD    IS        OMITTED.
     COPY        FIT00801  OF        XMDLIB.
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
     03  MSL-ST            PIC X(02).
     03  MSL-ST1           PIC X(04).
     03  TOK-ST            PIC X(02).
     03  TOK-ST1           PIC X(04).
     03  PRT-ST            PIC X(02).
     03  PRT-ST1           PIC X(04).
     03  TAN-ST            PIC X(02).
     03  TAN-ST1           PIC X(04).
*システム日付
*01  SYS-DATE              PIC 9(06).
*01  SYS-DATE8.
*    03  WSYS-YY           PIC 9(04).
*    03  WSYS-MM           PIC 9(02).
*    03  WSYS-DD           PIC 9(02).
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
 01  WK-AREA.
     03  END-SW            PIC 9(01) VALUE     ZERO.
     03  ERR-SW            PIC 9(01) VALUE     ZERO.
     03  PAGE-CNT          PIC 9(05) VALUE     ZERO.
     03  LINE-CNT          PIC 9(05) VALUE     ZERO.
     03  WK-TORICD         PIC 9(08) VALUE     ZERO.
*
 01  READ-CNT              PIC  9(07) VALUE 0.
 01  IN-CNT                PIC  9(07) VALUE 0.
 01  IN-DATA               PIC X(01).
 01  FILE-ERR.
     03  MSL-ERR           PIC  N(11) VALUE
                        NC"マスタ更新履歴Ｆエラー".
     03  TAN-ERR           PIC N(10) VALUE
                        NC"担当者マスタエラー".
     03  TOK-ERR           PIC N(10) VALUE
                        NC"取引先マスタエラー".
     03  PRT-ERR           PIC N(10) VALUE
                        NC"プリンターエラー".
 01  WK-ERR.
     03  WK-ERR1                  PIC  N(12) VALUE
              NC"　　終了が開始より小さい".
     03  WK-ERR2                  PIC  N(12) VALUE
              NC"　　『店舗マスタ作成中』".
     03  WK-ERR4                  PIC  N(12) VALUE
              NC"　対象データがありません".
*---------------- 99/09/28追加 --------------------------*
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*--------------------------------------------------------*
*帳票　更新履歴編集
 01  HEN-KUBUN.
     03  FILLER                   PIC  N(01)  VALUE NC"［".
     03  HEN-KUBUNNM              PIC  N(02).
     03  FILLER                   PIC  N(01)  VALUE NC"：".
 01  HEN-TAN.
     03  HEN-TANNM                PIC  N(10).
     03  FILLER                   PIC  N(01)  VALUE NC"：".
*担当者コード
 01  WK-TANCD.
     03  WK-TANCD1         PIC  X(02).
     03  WK-FILLER         PIC  X(06).
*マスタレコードエリア（店舗マスタ）
     COPY    HTENMS        OF   XFDLIB
     JOINING TEN           AS   PREFIX.
*更新範囲
 01  TRND-DT.
     03  TRND-DATE         PIC  9(08).
     03  TRND-TIME         PIC  9(06).
 01  TO-DT.
     03  TO-DATE           PIC  9(08).
     03  TO-TIME           PIC  9(06).
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
*
 PROC-010.
     PERFORM     INIT-SEC.
     PERFORM     MAIN-SEC  UNTIL     END-SW = 9  OR 1
     PERFORM     END-SEC.
     IF  (END-SW = 1)
         GO               TO         PROC-010.
*
     STOP        RUN.
 PROC-EXIT.
     EXIT.
**********************************************************
*                      Ｉ Ｎ Ｉ Ｔ                       *
**********************************************************
 INIT-SEC                  SECTION.
     MOVE        ZERO      TO        PAGE-CNT.
     MOVE        61        TO        LINE-CNT.
*----------------- 99/09/28追加 ---------------------*
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
*----------------------------------------------------*
     OPEN        INPUT     MSTLOGF  HTANMS
                           HTOKMS
                 OUTPUT    PRTF.
************
*帳票初期化*
************
     MOVE        SPACE     TO        FIT00801.
     INITIALIZE  FIT00801.
*マスタ更新履歴ファイル初期ＲＥＡＤ
     MOVE     "03"                TO   MSL-F01.
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
     IF  (LINE-CNT  >  60)
          PERFORM MIDA-SEC
     END-IF.
     PERFORM     MEIEDT-SEC.
     PERFORM     MEIWRT-SEC.
     PERFORM     READMSL-SEC.
*
 MAIN-EXIT.
     EXIT.
**********************
*マスタ更新履歴ファイルＲＥＡＤ*
**********************
 READMSL-SEC               SECTION.
     READ   MSTLOGF  NEXT  AT        END
         MOVE    9         TO        END-SW
         GO                TO        READMSL-EXIT.
*
     MOVE   MSL-F05        TO        TRND-DATE.
     MOVE   MSL-F06        TO        TRND-TIME.
     MOVE   PARA-UPDTDATE-E TO       TO-DATE.
     MOVE   PARA-UPDTIME-E  TO       TO-TIME.
*
     IF  (MSL-F01  >  "03") OR (MSL-F02  >  PARA-BUMONCD) OR
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
     CLOSE       PRTF      MSTLOGF  HTANMS  HTOKMS.
     DISPLAY  "SIT0080L READ =" READ-CNT UPON STA.
     DISPLAY  "SIT0080L IN   =" IN-CNT   UPON STA.
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
     MOVE        ZERO      TO        WK-TORICD.
     MOVE        ZERO      TO        LINE-CNT.
     MOVE        PAGE-CNT  TO        LPAGE.
**************
*帳票書き出し*
**************
     MOVE       "FIT00801" TO        PRT-FORM.
     MOVE       "GRP001"   TO        PRT-GRP.
     MOVE       "A000"     TO        PRT-CTL.
     MOVE       "PW"       TO        PRT-PROC.
     WRITE       FIT00801.
     ADD         7         TO        LINE-CNT.
 MIDA-EXIT.
     EXIT.
**********************************************************
*                 明細情報の編集                         *
**********************************************************
 MEIEDT-SEC                  SECTION.
*
     ADD         1         TO        IN-CNT.
     MOVE        MSL-F08   TO        TEN-REC.
     MOVE        TEN-F52   TO        TORICD.
*取引先ＲＥＡＤ
     MOVE        TEN-F52   TO        TOK-F01.
     READ    HTOKMS
     INVALID
          MOVE   SPACE     TO        TORINM
     NOT INVALID
          MOVE   TOK-F03   TO        TORINM
     END-READ.
*
 MEIEDT-010.
     MOVE        TEN-F011  TO        TENPCD.
     MOVE        TEN-F02   TO        TNAME.
     MOVE        TEN-F04   TO        TKANA.
*---------------- 99/09/28追加 -----------------*
     MOVE        TEN-F781  TO        NYBN1.
     MOVE        TEN-F782  TO        NYBN2.
*-----------------------------------------------*
     MOVE        TEN-F05   TO        YUBIN.
     MOVE        TEN-F06   TO        JYU1.
     MOVE        TEN-F07   TO        JYU2.
     MOVE        TEN-F08   TO        TEL.
     MOVE        TEN-F09   TO        FAX.
     MOVE        TEN-F97   TO        KBN.
*    MOVE        TEN-F75   TO        CENT.
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
     MOVE       "FIT00801" TO        PRT-FORM.
     WRITE       FIT00801.
*帳票初期化*
     MOVE        SPACE     TO        FIT00801.
     INITIALIZE  FIT00801.
     ADD         4         TO        LINE-CNT.
 MEIWRT-EXIT.
     EXIT.

```
