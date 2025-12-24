****************************************************************
*
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　
*    サブシステム　　　　：　出荷連携サブシステム
*    業務名　　　　　　　：　           　　　　　　　
*    モジュール名　　　　：　オンライン連携状況リスト発行
*    作成日／作成者　　　：　12/10/04  /M.T
*    処理概要　　　　　　：　オンライン連携状況のリスト　　　
*    更新履歴　　　　　　：
*
****************************************************************
 IDENTIFICATION            DIVISION.
 PROGRAM-ID.               SBT0110L.
 AUTHOR.                   M.T.
 DATE-WRITTEN.             12/10/04.
 ENVIRONMENT               DIVISION.
 CONFIGURATION             SECTION.
 SOURCE-COMPUTER.
 OBJECT-COMPUTER.
 SPECIAL-NAMES.
     STATION     IS        STA
     YA          IS        NIHONGO
     YB          IS        YB
     YB-21       IS        YB-21
     YB-22       IS        YB-22
     CONSOLE     IS        CONS.
***********************************************************
*             INPUT-OUTPUT                                *
***********************************************************
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*オンライン連携状況ファイル
     SELECT     LNKONLF    ASSIGN    TO        LNKONLL2
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      SEQUENTIAL
                           RECORD    KEY       LNK-F04
                                               LNK-F01
                                               LNK-F02
                                               LNK-F03
                                               LNK-F05
                     FILE      STATUS    LNK-ST.
*取引先マスタ
     SELECT     HTOKMS     ASSIGN    TO        TOKMS2
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       TOK-F01
                           FILE      STATUS    TOK-ST.
*担当者マスタ
     SELECT     HTANMS     ASSIGN    TO        TANMS1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       TAN-F01
                                               TAN-F02
                           FILE      STATUS    TAN-ST.
*倉庫マスタ
     SELECT     ZSOKMS     ASSIGN    TO        ZSOKMS1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       SOK-F01
                           FILE      STATUS    SOK-ST.
*%* プリンター *%*
     SELECT     PRTF       ASSIGN    TO        LP-04.
******************************************************************
*             DATA                DIVISION                       *
******************************************************************
 DATA                      DIVISION.
 FILE                      SECTION.
*オンライン連携状況ファイル
 FD  LNKONLF
     BLOCK       CONTAINS   8        RECORDS
     LABEL       RECORD    IS        STANDARD.
     COPY        LNKONLF   OF        XFDLIB
     JOINING     LNK       AS        PREFIX.
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

*担当者マスタ
 FD  HTANMS
     BLOCK       CONTAINS   8        RECORDS
     LABEL       RECORD    IS        STANDARD.
     COPY        HTANMS    OF        XFDLIB
     JOINING     TAN       AS        PREFIX.
*プリンター
 FD    PRTF      LINAGE  IS  66.
 01    P-REC                 PIC  X(200).
******************************************************************
 WORKING-STORAGE           SECTION.
******************************************************************
*
 01  FILE-STATUS.
     03  LNK-ST            PIC X(02).
     03  LNK-ST1           PIC X(04).
     03  PRT-ST            PIC X(02).
     03  PRT-ST1           PIC X(04).
     03  SOK-ST            PIC X(02).
     03  TAN-ST            PIC X(02).
     03  TOK-ST            PIC X(02).
*年度
 01  YYYY.
     03  YY1               PIC  9(02).
     03  YY2               PIC  9(02).
 01  WK-AREA.
     03  END-SW            PIC 9(01) VALUE     ZERO.
     03  ERR-SW            PIC 9(01) VALUE     ZERO.
     03  PAGE-CNT          PIC 9(05) VALUE     ZERO.
     03  LINE-CNT          PIC 9(05) VALUE     ZERO.
     03  WK-NYUKIN         PIC 9(02) VALUE     ZERO.
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
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*
 01  IN-DATA               PIC X(01).
 01  FILE-ERR.
     03  LNK-ERR           PIC  N(11) VALUE
                        NC"オンライン連携Ｆエラー".
     03  TAN-ERR           PIC N(10) VALUE
                        NC"担当者マスタエラー".
     03  PRT-ERR           PIC N(10) VALUE
                        NC"プリンターエラー".
     03  SOK-ERR           PIC N(10) VALUE
                        NC"倉庫マスタエラー".
     03  TOK-ERR           PIC N(10) VALUE
                        NC"取引先マスタエラー".
*帳票表示日付編集
 01  HEN-DATE.
     03  HEN-DATE-YYYY            PIC  9(04)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-DD              PIC  9(02)  VALUE  ZERO.
*帳票表示時刻編集
 01  HEN-TIME.
     03  HEN-TIME-HH              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-SS              PIC  9(02)  VALUE  ZERO.
*退避エリア
 01  WK-BMNCD-BK           PIC  X(04).
 01  WK-TANCD-BK           PIC  X(02).
 01  WK-TANNM-BK           PIC  N(10).
 01  WK-TORCD-BK           PIC  X(08).
 01  WK-TORNM-BK           PIC  N(15).
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
 01  FG-ZSOKMS-INV         PIC  X(03) VALUE SPACE.
 01  FG-HTANMS-INV         PIC  X(03) VALUE SPACE.
 01  FG-HTOKMS-INV         PIC  X(03) VALUE SPACE.
 01  FG-LNKONLF-INV        PIC  X(03) VALUE SPACE.
 01  SET-FLG               PIC  X(03) VALUE SPACE.
*帳票出力定義エリア
****  見出し行１             ****
 01  MIDASI-1.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  X(08)  VALUE  "SBT0110L".
     02  FILLER              PIC  X(27)  VALUE  SPACE.
     02  FILLER              PIC  N(14)
         CHARACTER  TYPE  IS  YB-22      VALUE
         NC"＜オンライン連携状況リスト＞".
     02  FILLER              PIC  X(36)  VALUE  SPACE.
     02  SYSYY               PIC  9999.
     02  FILLER              PIC  N(01)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"年".
     02  SYSMM               PIC  99.
     02  FILLER              PIC  N(01)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"月".
     02  SYSDD               PIC  99.
     02  FILLER              PIC  N(01)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"日".
     02  FILLER              PIC  X(02) VALUE  SPACE.
     02  LPAGE               PIC  ZZ9.
     02  FILLER              PIC  N(01)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"頁".
****  見出し行２***
 01  MIDASI-2.
     02  FILLER              PIC  X(116) VALUE  SPACE.
     02  TIMEHH              PIC  99.
     02  FILLER              PIC  X(01)  VALUE  ":".
     02  TIMEMM              PIC  99.
     02  FILLER              PIC  X(01)  VALUE  ":".
     02  TIMESS              PIC  99.
****  見出し行３           ****
 01  MIDASI-3.
     02  FILLER              PIC  X(136) VALUE  ALL "-".

***  見出し行４
 01  MIDASI-4.
     02  FILLER              PIC  X(03) VALUE  SPACE.
     02  FILLER              PIC  N(06) VALUE
         NC"照会倉庫　："  CHARACTER  TYPE  IS  YB.
     02  FILLER              PIC  X(01) VALUE  SPACE.
     02  SOKOCD              PIC  X(02).
     02  FILLER              PIC  X(01) VALUE  SPACE.
     02  SOKONM              PIC  N(18) CHARACTER  TYPE  IS  YB.
*
***  見出し行５
 01  MIDASI-5.
     02  FILLER              PIC  X(03) VALUE  SPACE.
     02  FILLER              PIC  N(06) VALUE
         NC"照会区分　："  CHARACTER  TYPE  IS  YB.
     02  FILLER              PIC  X(01) VALUE  SPACE.
     02  SHOKBNM             PIC  N(03) CHARACTER  TYPE  IS  YB.
*
***  見出し行６
 01  MIDASI-6.
     02  FILLER              PIC  X(03) VALUE  SPACE.
     02  FILLER              PIC  N(06) VALUE
         NC"基準受信日："  CHARACTER  TYPE  IS  YB.
     02  FILLER              PIC  X(01) VALUE  SPACE.
     02  KJNJBI              PIC  X(10).
*
***  見出し行７
 01  MIDASI-7.
     02  FILLER              PIC  X(03) VALUE  SPACE.
     02  FILLER              PIC  N(06) VALUE
         NC"指定取引先："  CHARACTER  TYPE  IS  YB.
     02  FILLER              PIC  X(01) VALUE  SPACE.
     02  STORCD              PIC  X(08).
     02  FILLER              PIC  X(01) VALUE  SPACE.
     02  STORNM              PIC  N(15) CHARACTER  TYPE  IS  YB.

****  見出し行８             ****
 01  MIDASI-8.
     02  FILLER              PIC  X(04)  VALUE  SPACE.
     02  FILLER              PIC  N(05)  VALUE
         NC"バッチＮＯ" CHARACTER  TYPE  IS  YB.
     02  FILLER              PIC  X(22)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE
         NC"納品日" CHARACTER  TYPE  IS  YB.
     02  FILLER              PIC  X(08)  VALUE  SPACE.
     02  FILLER              PIC  N(04)  VALUE
         NC"受信件数" CHARACTER  TYPE  IS  YB.
     02  FILLER              PIC  X(06)  VALUE  SPACE.
     02  FILLER              PIC  N(04)  VALUE
         NC"伝票件数" CHARACTER  TYPE  IS  YB.
     02  FILLER              PIC  X(06)  VALUE  SPACE.
     02  FILLER              PIC  N(04)  VALUE
         NC"連携済数" CHARACTER  TYPE  IS  YB.
     02  FILLER              PIC  X(06)  VALUE  SPACE.
     02  FILLER              PIC  N(04)  VALUE
         NC"未連携数" CHARACTER  TYPE  IS  YB.
     02  FILLER              PIC  X(03)  VALUE  SPACE.
     02  FILLER              PIC  N(05)  VALUE
         NC"最終連携日"       CHARACTER  TYPE  IS  YB.
     02  FILLER              PIC  X(04)  VALUE  SPACE.
     02  FILLER              PIC  N(05)  VALUE
         NC"最終連携者" CHARACTER  TYPE  IS  YB.
****  明細行１               ****
 01  MEISAI-1       CHARACTER     TYPE   IS   YB.
     02  FILLER              PIC  X(04)  VALUE  SPACE.
     02  BTNO                PIC  X(25).
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  NOHNBI              PIC  X(10).
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  JKENSU              PIC  Z,ZZZ,ZZ9.
     02  FILLER              PIC  X(03)  VALUE  SPACE.
     02  DENMSU              PIC  Z,ZZZ,ZZ9.
     02  FILLER              PIC  X(03)  VALUE  SPACE.
     02  RNKZSU              PIC  Z,ZZZ,ZZ9.
     02  FILLER              PIC  X(03)  VALUE  SPACE.
     02  MIRNKS              PIC  Z,ZZZ,ZZ9.
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  RNKBI               PIC  X(10).
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  RNKBMN              PIC  X(04).
     02  FILLER              PIC  X(01)  VALUE  "-".
     02  RNKTAN              PIC  X(02).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  RNKSHA              PIC  N(10).
****  明細行２               ****
 01  MEISAI-2       CHARACTER     TYPE   IS   YB.
     02  FILLER              PIC  X(04)  VALUE  SPACE.
     02  TORNM               PIC  N(15).
*
 LINKAGE                   SECTION.
 01  PAR-CHKBN            PIC X(01).
 01  PAR-SSOKO            PIC X(02).
 01  PAR-SHOKBN           PIC 9(01).
 01  PAR-KJNJBI           PIC X(08).
 01  PAR-TORCD            PIC X(08).
******************************************************************
*             PROCEDURE           DIVISION                       *
******************************************************************
 PROCEDURE                 DIVISION USING PAR-CHKBN
                                          PAR-SSOKO
                                          PAR-SHOKBN
                                          PAR-KJNJBI
                                          PAR-TORCD.
 DECLARATIVES.
 LNK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE LNKONLF.
     DISPLAY     LNK-ERR   UPON      STA.
     DISPLAY     LNK-ST    UPON      STA.
     ACCEPT      IN-DATA   FROM      STA.
     MOVE        4000      TO        PROGRAM-STATUS.
     STOP        RUN.
 PRT-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE PRTF.
     DISPLAY     PRT-ERR   UPON      STA.
     DISPLAY     PRT-ST    UPON      STA.
     ACCEPT      IN-DATA   FROM      STA.
     STOP        RUN.
 SOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE ZSOKMS.
     DISPLAY     SOK-ERR   UPON      STA.
     DISPLAY     SOK-ST    UPON      STA.
     ACCEPT      IN-DATA   FROM      STA.
     MOVE        4000      TO        PROGRAM-STATUS.
     STOP        RUN.
 TAN-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HTANMS.
     DISPLAY     TAN-ERR   UPON      STA.
     DISPLAY     TAN-ST    UPON      STA.
     ACCEPT      IN-DATA   FROM      STA.
     MOVE        4000      TO        PROGRAM-STATUS.
     STOP        RUN.
 TOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HTOKMS.
     DISPLAY     TOK-ERR   UPON      STA.
     DISPLAY     TOK-ST    UPON      STA.
     ACCEPT      IN-DATA   FROM      STA.
     MOVE        4000      TO        PROGRAM-STATUS.
     STOP        RUN.
 END DECLARATIVES.
***********************************************************
*                     ＭＡＩＮ処理                        *
***********************************************************
 PROC-SEC                  SECTION.
*
 PROC-010.
     PERFORM     INIT-SEC.
     PERFORM     MAIN-SEC  UNTIL     END-SW = 9
     PERFORM     END-SEC.
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
*システム時刻取得
     ACCEPT    WK-TIME          FROM   TIME.
*
     OPEN        INPUT     LNKONLF ZSOKMS HTANMS HTOKMS
                 OUTPUT    PRTF.
*帳票初期化*
     PERFORM  HEAD-SET-SEC.
*オンライン連携状況Ｆ初期ＲＥＡＤ
     MOVE     PAR-SSOKO           TO   LNK-F04.
     MOVE     PAR-KJNJBI          TO   LNK-F01
     MOVE     ZERO                TO   LNK-F02.
     IF  PAR-TORCD = SPACE OR PAR-TORCD = "00000000"
        MOVE  ZERO                TO   LNK-F03
     ELSE
         MOVE     PAR-TORCD       TO   LNK-F03
     END-IF.
     MOVE     ZERO                TO   LNK-F05.

     START    LNKONLF            KEY IS >= LNK-F04
                                           LNK-F01
                                           LNK-F02
                                           LNK-F03
                                           LNK-F05
              INVALID  MOVE  9    TO   END-SW
                       GO TO      INIT-EXIT.
     PERFORM  READLNK-SEC.
*
 INIT-EXIT.
     EXIT.
***********************************************************
*                     ＭＡＩＮ処理                        *
***********************************************************
 MAIN-SEC                  SECTION.
*
     IF SET-FLG = "SET"
         PERFORM     MEIEDT-SEC
         IF  (LINE-CNT  >  60)
              PERFORM MIDA-SEC
         END-IF
*
         PERFORM     MEIWRT-SEC
     END-IF.
     PERFORM     READLNK-SEC.
*
 MAIN-EXIT.
     EXIT.
**********************
*オンライン連携状況ＦＲＥＡＤ*
**********************
 READLNK-SEC               SECTION.
     MOVE SPACE  TO  SET-FLG.
     READ   LNKONLF        AT        END
         MOVE    9         TO        END-SW
         GO                TO        READLNK-EXIT.
*
* 倉庫ＣＤ
     IF  LNK-F04 = PAR-SSOKO
         CONTINUE
     ELSE
         MOVE    9         TO    END-SW
         GO TO  READLNK-EXIT
     END-IF.
* 取引先ＣＤ
     IF  PAR-TORCD NOT = SPACE  AND PAR-TORCD NOT = "00000000"
         IF  LNK-F03 = PAR-TORCD
             CONTINUE
         ELSE
             GO TO  READLNK-EXIT
         END-IF
     END-IF.
* バッチ日付
     IF   LNK-F01 >= PAR-KJNJBI
         CONTINUE
     ELSE
         GO TO  READLNK-EXIT
     END-IF.
* 照会区分
     IF  PAR-SHOKBN = "2"
         IF LNK-F09  = 0
             CONTINUE
         ELSE
             GO TO  READLNK-EXIT
         END-IF
     END-IF.
     IF  PAR-SHOKBN = "3"
         IF LNK-F09  NOT = 0
             CONTINUE
         ELSE
             GO TO  READLNK-EXIT
         END-IF
     END-IF.
     MOVE "SET"  TO  SET-FLG.
     ADD    1    TO  READ-CNT.
*
 READLNK-EXIT.
     EXIT.
**********************************************************
*                       Ｅ Ｎ Ｄ                         *
**********************************************************
 END-SEC                   SECTION.
*
     CLOSE       PRTF    LNKONLF ZSOKMS HTANMS HTOKMS.
     DISPLAY  "SBT0110L READ =" READ-CNT UPON STA.
     DISPLAY  "SIT0110L IN   =" IN-CNT   UPON STA.
 END-EXIT.
     EXIT.
**********************************************************
*                 ヘッダデータ編集                      *
**********************************************************
 HEAD-SET-SEC                  SECTION.
*
     MOVE        WK-YS     TO        YY1.
     MOVE        WK-Y      TO        YY2.
     MOVE        YYYY      TO        SYSYY.
     MOVE        WK-M      TO        SYSMM.
     MOVE        WK-D      TO        SYSDD.
     MOVE        WK-TIME(1:2)  TO    TIMEHH.
     MOVE        WK-TIME(3:2)  TO    TIMEMM.
     MOVE        WK-TIME(5:2)  TO    TIMESS.
*
     MOVE        PAR-SSOKO    TO    SOKOCD  SOK-F01.
     PERFORM     RD-ZSOKMS-SEC.
     IF FG-ZSOKMS-INV = ZERO
        MOVE  SOK-F02      TO  SOKONM
     END-IF.
     EVALUATE    PAR-SHOKBN
         WHEN    1
                 MOVE NC"全て　"  TO   SHOKBNM
         WHEN    2
                 MOVE NC"連携済"  TO   SHOKBNM
         WHEN    3
                 MOVE NC"未連携"  TO   SHOKBNM
     END-EVALUATE.

     MOVE        PAR-TORCD  TO       STORCD  TOK-F01.
     PERFORM     RD-HTOKMS-SEC.
     IF FG-HTOKMS-INV = ZERO
        MOVE  TOK-F02      TO  STORNM
     END-IF.
     MOVE     PAR-KJNJBI(1:4)   TO  HEN-DATE-YYYY.
     MOVE     PAR-KJNJBI(5:2)   TO  HEN-DATE-MM.
     MOVE     PAR-KJNJBI(7:2)   TO  HEN-DATE-DD.
     MOVE     HEN-DATE          TO  KJNJBI.
*
 HEAD-SET-EXIT.
     EXIT.
**********************************************************
*                 見出しデータ編集書き出し               *
**********************************************************
 MIDA-SEC                  SECTION.
*
     IF  PAGE-CNT  >  ZERO
         MOVE       SPACE   TO      P-REC
         WRITE      P-REC   AFTER   PAGE
     END-IF.
*
     ADD         1         TO        PAGE-CNT.
     MOVE        ZERO      TO        LINE-CNT.
     MOVE        PAGE-CNT  TO        LPAGE.
**************
*帳票書き出し*
**************
     MOVE       SPACE   TO  P-REC.
     WRITE      P-REC   AFTER  2.
     WRITE      P-REC   FROM    MIDASI-1   AFTER  1.
     WRITE      P-REC   FROM    MIDASI-2   AFTER  1.
     WRITE      P-REC   FROM    MIDASI-3   AFTER  1.
     WRITE      P-REC   FROM    MIDASI-4   AFTER  2.
     WRITE      P-REC   FROM    MIDASI-5   AFTER  1.
     WRITE      P-REC   FROM    MIDASI-6   AFTER  1.
     WRITE      P-REC   FROM    MIDASI-7   AFTER  1.
     WRITE      P-REC   FROM    MIDASI-8   AFTER  2.
     MOVE       SPACE   TO  P-REC.
     WRITE      P-REC   AFTER  1.
*
     ADD         15        TO        LINE-CNT.
*
 MIDA-EXIT.
     EXIT.
**********************************************************
*                 明細情報の編集                         *
**********************************************************
 MEIEDT-SEC                  SECTION.
     MOVE        LNK-F01(1:4)   TO        BTNO(1:4).
     MOVE        "/"            TO        BTNO(5:1).
     MOVE        LNK-F01(5:2)   TO        BTNO(6:2).
     MOVE        "/"            TO        BTNO(8:1).
     MOVE        LNK-F01(7:2)   TO        BTNO(9:2).
     MOVE        "-"            TO        BTNO(11:1).
     MOVE        LNK-F02(1:2)   TO        BTNO(12:2).
     MOVE        ":"            TO        BTNO(14:1).
     MOVE        LNK-F02(3:2)   TO        BTNO(15:2).
     MOVE        "-"            TO        BTNO(17:1).
     MOVE        LNK-F03        TO        BTNO(18:8).
     IF  LNK-F03 = WK-TORCD-BK
         MOVE  WK-TORNM-BK  TO  TORNM
     ELSE
         MOVE  LNK-F03          TO  TOK-F01 WK-TORCD-BK
         PERFORM     RD-HTOKMS-SEC
         IF FG-HTOKMS-INV = ZERO
             MOVE  TOK-F02      TO  TORNM WK-TORNM-BK
     END-IF.
     MOVE        LNK-F05(1:4)   TO      NOHNBI(1:4).
     MOVE        "/"            TO      NOHNBI(5:1).
     MOVE        LNK-F05(5:2)   TO      NOHNBI(6:2).
     MOVE        "/"            TO      NOHNBI(8:1).
     MOVE        LNK-F05(7:2)   TO      NOHNBI(9:2).
     MOVE        LNK-F06        TO      JKENSU.
     MOVE        LNK-F07        TO      DENMSU.
     MOVE        LNK-F08        TO      RNKZSU.
     MOVE        LNK-F09        TO      MIRNKS.
     MOVE        LNK-F10(1:4)   TO      RNKBI(1:4).
     MOVE        "/"            TO      RNKBI(5:1).
     MOVE        LNK-F10(5:2)   TO      RNKBI(6:2).
     MOVE        "/"            TO      RNKBI(8:1).
     MOVE        LNK-F10(7:2)   TO      RNKBI(9:2).
     MOVE        LNK-F11        TO      RNKBMN.
     MOVE        LNK-F12        TO      RNKTAN.
     IF  LNK-F11 = WK-BMNCD-BK  AND LNK-F12 = WK-TANCD-BK
         MOVE  WK-TANNM-BK  TO  RNKSHA
     ELSE
         MOVE    LNK-F11        TO      TAN-F01 WK-BMNCD-BK
         MOVE    LNK-F12        TO      TAN-F02 WK-TANCD-BK
         PERFORM     RD-HTANMS-SEC
         IF FG-HTANMS-INV = ZERO
            MOVE  TAN-F03      TO  RNKSHA WK-TANNM-BK
         END-IF
     END-IF.
*
 MEIEDT-EXIT.
     EXIT.
**********************************************************
*                    明細データ書き出し                  *
**********************************************************
 MEIWRT-SEC                   SECTION.
*
     ADD        1          TO        IN-CNT.
**************
*帳票書き出し*
**************
     WRITE      P-REC   FROM    MEISAI-1   AFTER  1.
     WRITE      P-REC   FROM    MEISAI-2   AFTER  1.
     MOVE       SPACE   TO  P-REC.
     WRITE      P-REC   AFTER  1.
     ADD         4         TO        LINE-CNT.
*
 MEIWRT-EXIT.
     EXIT.
****************************************************************
*    担当者マスタ検索                                          *
****************************************************************
 RD-HTANMS-SEC          SECTION.

     READ  HTANMS
       INVALID
         MOVE  1                 TO  FG-HTANMS-INV
       NOT INVALID
         MOVE  ZERO              TO  FG-HTANMS-INV
     END-READ.

 RD-HTANMS-EXIT.
     EXIT.
****************************************************************
*    取引先マスタ検索                                          *
****************************************************************
 RD-HTOKMS-SEC          SECTION.

     READ  HTOKMS
       INVALID
         MOVE  1                 TO  FG-HTOKMS-INV
       NOT INVALID
         MOVE  ZERO              TO  FG-HTOKMS-INV
     END-READ.

 RD-HTOKMS-EXIT.
     EXIT.

****************************************************************
*    倉庫マスタ検索                                          *
****************************************************************
 RD-ZSOKMS-SEC          SECTION.

     READ  ZSOKMS
       INVALID
         MOVE  1                 TO  FG-ZSOKMS-INV
       NOT INVALID
         MOVE  ZERO              TO  FG-ZSOKMS-INV
     END-READ.

 RD-ZSOKMS-EXIT.
     EXIT.
