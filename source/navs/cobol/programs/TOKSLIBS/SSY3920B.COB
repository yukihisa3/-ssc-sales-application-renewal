****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　在庫ＥＸＣＥＬ連携　　　　　　　　*
*    モジュール名　　　　：　ナフコ商品分類マスタ取込チェック　*
*    作成日／作成者　　　：　2016/08/18 MIURA                  *
*    処理内容　　　　　　：　ナフコ商品分類ＥＸＣＥＬデータ　　*
*    　　　　　　　　　　　　について、整合性チェック・件数　　*
*                            カウントを行う。　　　　　　　　　*
*    変更日／作成者　　　：　                                  *
*    変更内容　　　　　　：                                   *
****************************************************************
 IDENTIFICATION              DIVISION.
 PROGRAM-ID.                 SSY3920B.
 ENVIRONMENT                 DIVISION.
 CONFIGURATION               SECTION.
 SOURCE-COMPUTER.
 OBJECT-COMPUTER.
 SPECIAL-NAMES.
     CONSOLE       IS        CONS
     STATION       IS        STAT.
****************************************************************
 INPUT-OUTPUT              SECTION.
****************************************************************
 FILE-CONTROL.
*ナフコ商品分類マスタＥＸＣＥＬデータ
     SELECT      NFSHBCSV    ASSIGN    TO       DA-01-S-NFSHBCSV
                             ORGANIZATION       SEQUENTIAL
                             ACCESS    MODE     SEQUENTIAL
                             FILE      STATUS   CSV-ST.
*ナフコ商品分類マスタＥＸＣＥＬ取込ファイル
     SELECT      NFSHBWK2    ASSIGN    TO       DA-01-VI-NFSHBWK2
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     DYNAMIC
                             RECORD    KEY      NFS-F051 NFS-F053
                                                NFS-F060
                                                WITH  DUPLICATES
                             FILE      STATUS   NFS-ST.
*条件ファイル
     SELECT      JYOKEN1     ASSIGN    TO       DA-01-VI-JYOKEN1
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     RANDOM
                             RECORD    KEY      JYO-F01 JYO-F02
                             FILE      STATUS   JYO-ST.
****************************************************************
 DATA                        DIVISION.
****************************************************************
 FILE                        SECTION.
*ナフコ商品分類マスタＥＸＣＥＬデータ
 FD  NFSHBCSV
     LABEL       RECORD      IS        STANDARD
     BLOCK       CONTAINS    93        RECORDS.
     COPY        NFSHBCSV    OF        XFDLIB
     JOINING     CSV         AS        PREFIX.
*ナフコ商品分類マスタＥＸＣＥＬ取込ファイル
 FD  NFSHBWK2.
     COPY        NFSHBWK2    OF        XFDLIB
     JOINING     NFS         AS        PREFIX.
*条件ファイル
 FD  JYOKEN1.
     COPY        JYOKEN1     OF        XFDLIB
     JOINING     JYO         AS        PREFIX.
****************************************************************
 WORKING-STORAGE           SECTION.
****************************************************************
 01  ST-AREA.
     03  CSV-ST              PIC  X(02)  VALUE  SPACE.
     03  NFS-ST              PIC  X(02)  VALUE  SPACE.
     03  JYO-ST              PIC  X(02)  VALUE  SPACE.
 01  WK-AREA.
     03  END-FLG             PIC  X(03)  VALUE  SPACE.
     03  CSV-ENDFLG          PIC  X(01)  VALUE  SPACE.
     03  CSV-CNT             PIC  9(07)  VALUE  ZERO.
     03  NFS-CNT             PIC  9(07)  VALUE  ZERO.
     03  ERR-CNT             PIC  9(07)  VALUE  ZERO.
     03  UNKNOWN             PIC  9(07)  VALUE  ZERO.
     03  OK-CNT              PIC  9(07)  VALUE  ZERO.
**
     03  WK-ERR-TBL.
         05  ERR-KBN         PIC  X(01)  OCCURS  10.
     03  WK-ERR-KBN          PIC  X(01).
*
 01  WK-CSV-F03              PIC  X(04)  VALUE SPACE.
 01  WK-CSV-F03-X            REDEFINES   WK-CSV-F03.
     03  WK-CSV-F03R         PIC  9(04).
*
 01  JYO-INV-FLG             PIC  X(03)  VALUE  SPACE.
**
*日付取得
 01  SYS-DATE                PIC  9(06)  VALUE  ZERO.
*
 01  SYS-DATE8               PIC  9(08)  VALUE  ZERO.
*
 01  WK-DATE8.
     03  WK-Y                PIC  9(04)  VALUE  ZERO.
     03  WK-M                PIC  9(02)  VALUE  ZERO.
     03  WK-D                PIC  9(02)  VALUE  ZERO.
*
*
 01  SYS-TIME                PIC  9(08).
 01  WK-TIME      REDEFINES  SYS-TIME.
   03  WK-TIME-HM            PIC  9(06).
   03  WK-TIME-FIL           PIC  X(02).
*
 01  SEC-NAME.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-SEC    => ".
     03  S-NAME                   PIC  X(20).
 01  FILE-ERR.
     03  CSV-ERR            PIC  N(10)  VALUE
                   NC"ＥＸＣＥＬデータ異常".
     03  NFS-ERR             PIC  N(10)  VALUE
                   NC"ＥＸＣＥＬ取込Ｆ異常".
     03  JYO-ERR             PIC  N(10)  VALUE
                   NC"条件ファイル異常".
*メッセージ出力領域
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  ST-PG           PIC  X(08)  VALUE "SSY3920B".
         05  FILLER          PIC  X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  END-PG          PIC  X(08)  VALUE "SSY3920B".
         05  FILLER          PIC  X(11)  VALUE
                                         " END   *** ".
     03  MSG-OUT1.
         05  MSG-OUT1-FIL1   PIC  X(02)  VALUE "##".
         05  MSG-OUT1-FIL2   PIC  N(13)  VALUE
                             NC"商品分類ＥＸＣＥＬ読込　＝".
         05  MSG-OUT01       PIC  ZZZ,ZZ9.
         05  MSG-OUT1-FIL3   PIC  X(01)  VALUE " ".
         05  MSG-OUT1-FIL4   PIC  N(01)  VALUE NC"件".
     03  MSG-OUT2.
         05  MSG-OUT2-FIL1   PIC  X(02)  VALUE "##".
         05  MSG-OUT2-FIL2   PIC  N(11)  VALUE
                             NC"取込ファイル作成　　＝".
         05  MSG-OUT02       PIC  ZZZ,ZZ9.
         05  MSG-OUT2-FIL3   PIC  X(01)  VALUE " ".
         05  MSG-OUT2-FIL4   PIC  N(01)  VALUE NC"件".
     03  MSG-OUT3.
         05  MSG-OUT3-FIL1   PIC  X(02)  VALUE "##".
         05  MSG-OUT3-FIL2   PIC  N(11)  VALUE
                             NC"　内．エラーデータ　＝".
         05  MSG-OUT03       PIC  ZZZ,ZZ9.
         05  MSG-OUT3-FIL3   PIC  X(01)  VALUE " ".
         05  MSG-OUT3-FIL4   PIC  N(01)  VALUE NC"件".
     03  MSG-OUT4.
         05  MSG-OUT4-FIL1   PIC  X(02)  VALUE "##".
         05  MSG-OUT4-FIL2   PIC  N(11)  VALUE
                             NC"　内．ＯＫデータ　　＝".
         05  MSG-OUT04       PIC  ZZZ,ZZ9.
         05  MSG-OUT4-FIL3   PIC  X(01)  VALUE " ".
         05  MSG-OUT4-FIL4   PIC  N(01)  VALUE NC"件".
*
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN             PIC X(01).
 01  LINK-IN-YMD6            PIC 9(06).
 01  LINK-IN-YMD8            PIC 9(08).
 01  LINK-OUT-RET            PIC X(01).
 01  LINK-OUT-YMD            PIC 9(08).
****************************************************************
 LINKAGE                     SECTION.
****************************************************************
 01  LINK-IN-BUMON               PIC  X(04).
 01  LINK-IN-TANCD               PIC  X(02).
 01  LINK-OUT-CNT1               PIC  9(07).
 01  LINK-OUT-CNT2               PIC  9(07).
****************************************************************
 PROCEDURE                   DIVISION  USING LINK-IN-BUMON
                                             LINK-IN-TANCD
                                             LINK-OUT-CNT1
                                             LINK-OUT-CNT2.
****************************************************************
 DECLARATIVES.
 CSV-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE NFSHBCSV.
     DISPLAY     CSV-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     CSV-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 NFS-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE NFSHBWK2.
     DISPLAY     NFS-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     NFS-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 JYO-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE JYOKEN1.
     DISPLAY     JYO-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     JYO-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 END DECLARATIVES.
****************************************************************
*                 P R O G R A M - S E C
****************************************************************
 PROGRAM-SEC                 SECTION.
     PERFORM     INIT-SEC.
     PERFORM     MAIN-SEC    UNTIL     END-FLG  =  "END".
     PERFORM     END-SEC.
     STOP        RUN.
*PROGRAM-END.
****************************************************************
*                 I N I T - S E C
****************************************************************
 INIT-SEC                    SECTION.
     MOVE       "INIT-SEC"   TO   S-NAME.
*
     DISPLAY     MSG-START   UPON  CONS.
*
     OPEN        INPUT       NFSHBCSV.
     OPEN        I-O         NFSHBWK2.
     OPEN        INPUT       JYOKEN1.
*
*システム日付・時刻の取得
     ACCEPT   SYS-DATE          FROM   DATE.
     ACCEPT   SYS-TIME          FROM   TIME.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     SYS-DATE            TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD       TO   WK-DATE8
                                       SYS-DATE8.
     DISPLAY "# HIDUKE = " WK-DATE8   UPON CONS.
     DISPLAY "# JIKAN  = " WK-TIME-HM UPON CONS.
*
*
 INIT-EXIT.
     EXIT.
****************************************************************
*    条件ファイル検索
****************************************************************
 JYO-READ-SEC               SECTION.
     MOVE     "JYO-READ-SEC" TO   S-NAME.
*
     READ     JYOKEN1
       INVALID
              MOVE "INV"     TO   JYO-INV-FLG
       NOT  INVALID
              MOVE SPACE     TO   JYO-INV-FLG
     END-READ.
 JYO-READ-EXIT.
     EXIT.
****************************************************************
*                 M A I N - S E C
****************************************************************
 MAIN-SEC                    SECTION.
     MOVE     "MAIN-SEC"          TO   S-NAME.
*
 MAIN-100.
* ナフコ商品分類マスタＥＸＣＥＬ読込　終了するまで繰りかえす。
     PERFORM  READ-CSV-SEC.
*
     IF       CSV-ENDFLG  =   SPACE
              PERFORM         HENSYU-CSV-SEC
              GO          TO  MAIN-100
     END-IF.
*
     MOVE    "END"        TO  END-FLG.
*
 MAIN-SEC-EXIT.
     EXIT.
****************************************************************
* ナフコ商品分類マスタＥＸＣＥＬ順読込
****************************************************************
 READ-CSV-SEC                SECTION.
     MOVE     "READ-CSV-SEC"      TO   S-NAME.
*
     READ     NFSHBCSV   AT   END
              MOVE      "Y"       TO   CSV-ENDFLG
              GO                  TO   READ-CSV-EXIT
     END-READ.
*カウント（取込件数）
     ADD      1                   TO   CSV-CNT.
*
 READ-CSV-EXIT.
     EXIT.
***************************************************************
*ナフコ商品分類マスタＥＸＣＥＬチェック編集
***************************************************************
 HENSYU-CSV-SEC             SECTION.
*
     MOVE    "HENSYU-CSV-SEC"      TO   S-NAME.
*
     MOVE     SPACE                TO   WK-ERR-KBN.
     INITIALIZE                         WK-ERR-TBL.
*
*データ項目チェック
     PERFORM   DATA-CHK-SEC.
*
*ナフコ商品分類マスタＥＸＣＥＬ取込ファイル出力
     PERFORM   NFS-WRITE-SEC.
*
 HENSYU-CSV-EXIT.
     EXIT.
***************************************************************
*             データ項目チェック
***************************************************************
 DATA-CHK-SEC     SECTION.
*
     MOVE   "DATA-CHK-SEC"       TO   S-NAME.
*
*------------------------------------------*
 DATA-CHK-00.
*【必須項目チェック】
*------------------------------------------*
*  『商品分類ＣＤ』
     IF  CSV-F01  =   SPACE
         MOVE   "Y"          TO        WK-ERR-KBN
         MOVE   "1"          TO        ERR-KBN(03)
     END-IF.
*
*  『商品分類名』
     IF  CSV-F02 =   SPACE
         MOVE   "Y"          TO        WK-ERR-KBN
         MOVE   "1"          TO        ERR-KBN(03)
     END-IF.
*
*  『パターンＣＤ』
     IF  CSV-F03 =   SPACE
         MOVE   "Y"          TO        WK-ERR-KBN
         MOVE   "1"          TO        ERR-KBN(03)
     END-IF.
*
*------------------------------------------*
 DATA-CHK-01.
*【商品分類CD未登録チェック】
*------------------------------------------*
     MOVE     5              TO   JYO-F01.
     MOVE     CSV-F01        TO   JYO-F02.
     PERFORM  JYO-READ-SEC.
     IF       JYO-INV-FLG    =    "INV"
              MOVE   "Y"     TO   WK-ERR-KBN
              MOVE   "1"     TO   ERR-KBN(01)
     END-IF.
*------------------------------------------*
 DATA-CHK-02.
*【整合性チェック】
*------------------------------------------*
     MOVE     SPACE          TO   NFS-F060.
     MOVE     CSV-F01        TO   NFS-F051.
     MOVE     CSV-F03        TO   NFS-F053.
*             DISPLAY   "NFS-F051=" NFS-F051
*                       "NFS-F053=" NFS-F053
*                       "NFS-F060=" NFS-F060
*                                                UPON CONS.
     START    NFSHBWK2   KEY  IS  >=   NFS-F051
                                       NFS-F053
                                       NFS-F060
       INVALID
         GO        TO    END-SEC-02
     END-START.
*
 READ-SEC-02.
     READ     NFSHBWK2   NEXT     AT   END
              GO        TO    END-SEC-02
     END-READ.
*
 UPDATA-SEC-02.
*
     IF    (  NFS-F051   = CSV-F01 ) AND
           (  NFS-F053   = CSV-F03 )
              MOVE   "Y"          TO        WK-ERR-KBN
              MOVE   "1"          TO        ERR-KBN(02)
              IF         NFS-F060  NOT = "1"
                         ADD       1     TO        ERR-CNT
                         SUBTRACT  1     FROM      OK-CNT
              END-IF
              MOVE      "1"        TO    NFS-F060  NFS-F062
              MOVE      SPACE      TO    NFS-F06A
              REWRITE    NFS-REC
              READ       NFSHBWK2
                   NEXT  AT   END
                         GO        TO    END-SEC-02
                   NOT   AT   END
                         GO        TO    READ-SEC-02
              END-READ
     ELSE
              GO        TO    END-SEC-02
     END-IF.
*
 END-SEC-02.
*
     CLOSE         NFSHBWK2.
     OPEN     I-O  NFSHBWK2.
*
*------------------------------------------*
 DATA-CHK-04.
*【カウント（エラー件数）】
*------------------------------------------*
     IF       WK-ERR-KBN  =   "Y"
              ADD    1        TO     ERR-CNT
     ELSE
              ADD    1        TO     OK-CNT
              MOVE   "1"      TO     ERR-KBN(10)
     END-IF.
*
 DATA-CHK-EXIT.
     EXIT.
*
****************************************************************
*  ナフコ商品分類マスタＥＸＣＥＬ取込ファイル出力
****************************************************************
 NFS-WRITE-SEC               SECTION.
*
     MOVE       "NFS-WRITE-SEC"    TO   S-NAME.
*
     MOVE     SPACE             TO        NFS-REC.
     INITIALIZE                           NFS-REC.
*
* 取込日付
     MOVE     SYS-DATE8         TO        NFS-F01.
* 取込時刻
     MOVE     WK-TIME-HM        TO        NFS-F02.
* 取込担当者部門ＣＤ
     MOVE     LINK-IN-BUMON     TO        NFS-F03.
* 取込担当者ＣＤ
     MOVE     LINK-IN-TANCD     TO        NFS-F04.
* 商品分類ＣＤ
     MOVE     CSV-F01           TO        NFS-F051.
* 商品分類名
     MOVE     CSV-F02           TO        NFS-F052.
* パターンＣＤ
     MOVE     CSV-F03           TO        NFS-F053.
* エラー区分
     IF   WK-ERR-KBN    NOT =   SPACE
          MOVE   "1"            TO        NFS-F060
     END-IF.
     MOVE    ERR-KBN(01)   TO        NFS-F061.
     MOVE    ERR-KBN(02)   TO        NFS-F062.
     MOVE    ERR-KBN(03)   TO        NFS-F063.
     MOVE    ERR-KBN(04)   TO        NFS-F064.
     MOVE    ERR-KBN(05)   TO        NFS-F065.
     MOVE    ERR-KBN(06)   TO        NFS-F066.
     MOVE    ERR-KBN(07)   TO        NFS-F067.
     MOVE    ERR-KBN(08)   TO        NFS-F068.
     MOVE    ERR-KBN(09)   TO        NFS-F069.
     MOVE    ERR-KBN(10)   TO        NFS-F06A.
*
     WRITE    NFS-REC.
     ADD      1                  TO   NFS-CNT.
*
 WRITE-NFS-EXIT.
     EXIT.
****************************************************************
*    終了
****************************************************************
 END-SEC                   SECTION.
*
     MOVE     "END-SEC"     TO    S-NAME.
*
     MOVE      CSV-CNT      TO    MSG-OUT01.
     MOVE      NFS-CNT      TO    MSG-OUT02.
     MOVE      ERR-CNT      TO    MSG-OUT03.
     MOVE      OK-CNT       TO    MSG-OUT04.
     DISPLAY   MSG-OUT1-FIL1 MSG-OUT1-FIL2 MSG-OUT01
               MSG-OUT1-FIL3 MSG-OUT1-FIL4 UPON CONS.
     DISPLAY   MSG-OUT2-FIL1 MSG-OUT2-FIL2 MSG-OUT02
               MSG-OUT2-FIL3 MSG-OUT2-FIL4 UPON CONS.
     DISPLAY   MSG-OUT3-FIL1 MSG-OUT3-FIL2 MSG-OUT03
               MSG-OUT3-FIL3 MSG-OUT3-FIL4 UPON CONS.
     DISPLAY   MSG-OUT4-FIL1 MSG-OUT4-FIL2 MSG-OUT04
               MSG-OUT4-FIL3 MSG-OUT4-FIL4 UPON CONS.
*
     CLOSE     NFSHBCSV
               NFSHBWK2
               JYOKEN1.
*
*ＯＵＴパラメタセット
* 取込件数
     MOVE      CSV-CNT      TO    LINK-OUT-CNT1.
* エラー件数
     MOVE      ERR-CNT      TO    LINK-OUT-CNT2.
*
     DISPLAY   MSG-END      UPON  CONS.
*
 END-EXIT.
     EXIT.
