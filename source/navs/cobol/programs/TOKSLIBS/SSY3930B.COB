****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　在庫ＥＸＣＥＬ連携　　　　　　　　*
*    モジュール名　　　　：　ナフコ店舗マスタ取込チェック　*
*    作成日／作成者　　　：　2016/08/18 MIURA                  *
*    処理内容　　　　　　：　ナフコ店舗ＥＸＣＥＬデータ　　*
*    　　　　　　　　　　　　について、整合性チェック・件数　　*
*                            カウントを行う。　　　　　　　　　*
*    変更日／作成者　　　：　                                  *
*    変更内容　　　　　　：                                   *
****************************************************************
 IDENTIFICATION              DIVISION.
 PROGRAM-ID.                 SSY3930B.
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
*ナフコ店舗マスタＥＸＣＥＬデータ
     SELECT      NFTEPCSV    ASSIGN    TO       DA-01-S-NFTEPCSV
                             ORGANIZATION       SEQUENTIAL
                             ACCESS    MODE     SEQUENTIAL
                             FILE      STATUS   CSV-ST.
*ナフコ店舗マスタＥＸＣＥＬ取込ファイル
     SELECT      NFTEPWK2    ASSIGN    TO       DA-01-VI-NFTEPWK2
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     DYNAMIC
                             RECORD    KEY      NFT-F051 NFT-F052
                                                NFT-F053 NFT-F060
                                                WITH  DUPLICATES
                             FILE      STATUS   NFT-ST.
*ナフコ店舗マスタ
     SELECT      NFTENMS1    ASSIGN    TO       DA-01-VI-NFTENMS1
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     RANDOM
                             RECORD    KEY      MS1-F01 MS1-F02
                             FILE      STATUS   MS1-ST.
*ナフコ店舗分類パターンマスタ
     SELECT      NFSHBPL1    ASSIGN    TO       DA-01-VI-NFSHBPL1
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     RANDOM
                             RECORD    KEY      PL1-F01 PL1-F03
                             FILE      STATUS   PL1-ST.
****************************************************************
 DATA                        DIVISION.
****************************************************************
 FILE                        SECTION.
*ナフコ店舗マスタＥＸＣＥＬデータ
 FD  NFTEPCSV
     LABEL       RECORD      IS        STANDARD
     BLOCK       CONTAINS    240       RECORDS.
     COPY        NFTEPCSV    OF        XFDLIB
     JOINING     CSV         AS        PREFIX.
*ナフコ店舗マスタＥＸＣＥＬ取込ファイル
 FD  NFTEPWK2.
     COPY        NFTEPWK2    OF        XFDLIB
     JOINING     NFT         AS        PREFIX.
*ナフコ店舗マスタ
 FD  NFTENMS1.
     COPY        NFTENMS1    OF        XFDLIB
     JOINING     MS1         AS        PREFIX.
*ナフコ店舗分類パターンマスタ
 FD  NFSHBPL1.
     COPY        NFSHBPL1    OF        XFDLIB
     JOINING     PL1         AS        PREFIX.
****************************************************************
 WORKING-STORAGE           SECTION.
****************************************************************
 01  ST-AREA.
     03  CSV-ST              PIC  X(02)  VALUE  SPACE.
     03  NFT-ST              PIC  X(02)  VALUE  SPACE.
     03  MS1-ST              PIC  X(02)  VALUE  SPACE.
     03  PL1-ST              PIC  X(02)  VALUE  SPACE.
 01  WK-AREA.
     03  END-FLG             PIC  X(03)  VALUE  SPACE.
     03  CSV-ENDFLG          PIC  X(01)  VALUE  SPACE.
     03  CSV-CNT             PIC  9(07)  VALUE  ZERO.
     03  NFT-CNT             PIC  9(07)  VALUE  ZERO.
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
 01  MS1-INV-FLG             PIC  X(03)  VALUE  SPACE.
 01  PL1-INV-FLG             PIC  X(03)  VALUE  SPACE.
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
     03  NFT-ERR             PIC  N(10)  VALUE
                   NC"ＥＸＣＥＬ取込Ｆ異常".
     03  MS1-ERR             PIC  N(10)  VALUE
                   NC"店舗マスタ異常".
     03  PL1-ERR             PIC  N(10)  VALUE
                   NC"商品分類異常".
*メッセージ出力領域
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  ST-PG           PIC  X(08)  VALUE "SSY3930B".
         05  FILLER          PIC  X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  END-PG          PIC  X(08)  VALUE "SSY3930B".
         05  FILLER          PIC  X(11)  VALUE
                                         " END   *** ".
     03  MSG-OUT1.
         05  MSG-OUT1-FIL1   PIC  X(02)  VALUE "##".
         05  MSG-OUT1-FIL2   PIC  N(13)  VALUE
                             NC"店舗ＥＸＣＥＬ読込　＝".
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
     USE         AFTER       EXCEPTION PROCEDURE NFTEPCSV.
     DISPLAY     CSV-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     CSV-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 NFT-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE NFTEPWK2.
     DISPLAY     NFT-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     NFT-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 MS1-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE NFTENMS1.
     DISPLAY     MS1-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     MS1-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 PL1-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE NFSHBPL1.
     DISPLAY     PL1-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     PL1-ST      UPON      CONS.
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
     OPEN        INPUT       NFTEPCSV NFTENMS1 NFSHBPL1.
     OPEN        I-O         NFTEPWK2.
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
*    店舗マスタ検索
****************************************************************
 MS1-READ-SEC               SECTION.
     MOVE     "MS1-READ-SEC" TO   S-NAME.
*
     READ     NFTENMS1
       INVALID
              MOVE "INV"     TO   MS1-INV-FLG
       NOT  INVALID
              MOVE SPACE     TO   MS1-INV-FLG
     END-READ.
 MS1-READ-EXIT.
     EXIT.
****************************************************************
*    商品分類パターンマスタ検索
****************************************************************
 PL1-READ-SEC               SECTION.
     MOVE     "PL1-READ-SEC" TO   S-NAME.
*
     READ     NFSHBPL1
       INVALID
              MOVE "INV"     TO   PL1-INV-FLG
       NOT  INVALID
              MOVE SPACE     TO   PL1-INV-FLG
     END-READ.
 PL1-READ-EXIT.
     EXIT.
****************************************************************
*                 M A I N - S E C
****************************************************************
 MAIN-SEC                    SECTION.
     MOVE     "MAIN-SEC"          TO   S-NAME.
*
 MAIN-100.
* ナフコ店舗マスタＥＸＣＥＬ読込　終了するまで繰りかえす。
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
* ナフコ店舗マスタＥＸＣＥＬ順読込
****************************************************************
 READ-CSV-SEC                SECTION.
     MOVE     "READ-CSV-SEC"      TO   S-NAME.
*
     READ     NFTEPCSV   AT   END
              MOVE      "Y"       TO   CSV-ENDFLG
              GO                  TO   READ-CSV-EXIT
     END-READ.
*カウント（取込件数）
     ADD      1                   TO   CSV-CNT.
*
     IF  CSV-CNT(5:3) = "000" OR "500"
         DISPLAY "READ-CNT = " CSV-CNT  UPON  CONS
     END-IF.
*
 READ-CSV-EXIT.
     EXIT.
***************************************************************
*ナフコ店舗マスタＥＸＣＥＬチェック編集
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
*ナフコ店舗マスタＥＸＣＥＬ取込ファイル出力
     PERFORM   NFT-WRITE-SEC.
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
*  『店舗ＣＤ』
     IF  CSV-F01  =   ZERO
         MOVE   "Y"          TO        WK-ERR-KBN
         MOVE   "1"          TO        ERR-KBN(04)
     END-IF.
*
*  『商品分類ＣＤ』
     IF  CSV-F02 =   SPACE
         MOVE   "Y"          TO        WK-ERR-KBN
         MOVE   "1"          TO        ERR-KBN(04)
     END-IF.
*
*  『パターンＣＤ』
     IF  CSV-F03 =   SPACE
         MOVE   "Y"          TO        WK-ERR-KBN
         MOVE   "1"          TO        ERR-KBN(04)
     END-IF.
*
*------------------------------------------*
 DATA-CHK-01.
*【店舗ＣＤ未登録チェック】
*------------------------------------------*
     MOVE     137607         TO   MS1-F01.
     MOVE     CSV-F01        TO   MS1-F02.
     PERFORM  MS1-READ-SEC.
     IF       MS1-INV-FLG    =    "INV"
              MOVE   "Y"     TO   WK-ERR-KBN
              MOVE   "1"     TO   ERR-KBN(01)
     END-IF.
*------------------------------------------*
 DATA-CHK-02.
*【商品分類CD未登録チェック】
*------------------------------------------*
     MOVE     CSV-F02        TO   PL1-F01.
     MOVE     CSV-F03        TO   PL1-F03.
     PERFORM  PL1-READ-SEC.
     IF       PL1-INV-FLG    =    "INV"
              MOVE   "Y"     TO   WK-ERR-KBN
              MOVE   "1"     TO   ERR-KBN(02)
     END-IF.
*------------------------------------------*
 DATA-CHK-03.
*【整合性チェック】
*------------------------------------------*
     MOVE     SPACE          TO   NFT-F060.
     MOVE     CSV-F01        TO   NFT-F051.
     MOVE     CSV-F02        TO   NFT-F052.
     MOVE     CSV-F03        TO   NFT-F053.
     START    NFTEPWK2   KEY  IS  >=   NFT-F051
                                       NFT-F052
                                       NFT-F053
                                       NFT-F060
       INVALID
         GO        TO    END-SEC-03
     END-START.
*
 READ-SEC-03.
     READ     NFTEPWK2   NEXT     AT   END
              GO        TO    END-SEC-03
     END-READ.
*
 UPDATA-SEC-03.
*
     IF    (  NFT-F051   = CSV-F01 ) AND
           (  NFT-F052   = CSV-F02 ) AND
           (  NFT-F053   = CSV-F03 )
              MOVE   "Y"          TO        WK-ERR-KBN
              MOVE   "1"          TO        ERR-KBN(03)
              IF         NFT-F060  NOT = "1"
                         ADD       1     TO        ERR-CNT
                         SUBTRACT  1     FROM      OK-CNT
              END-IF
              MOVE      "1"        TO    NFT-F060  NFT-F063
              MOVE      SPACE      TO    NFT-F06A
              REWRITE    NFT-REC
              READ       NFTEPWK2
                   NEXT  AT   END
                         GO        TO    END-SEC-03
                   NOT   AT   END
                         GO        TO    READ-SEC-03
              END-READ
     END-IF.
*
 END-SEC-03.
*
     CLOSE         NFTEPWK2.
     OPEN     I-O  NFTEPWK2.
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
*  ナフコ店舗マスタＥＸＣＥＬ取込ファイル出力
****************************************************************
 NFT-WRITE-SEC               SECTION.
*
     MOVE       "NFT-WRITE-SEC"    TO   S-NAME.
*
     MOVE     SPACE             TO        NFT-REC.
     INITIALIZE                           NFT-REC.
*
* 取込日付
     MOVE     SYS-DATE8         TO        NFT-F01.
* 取込時刻
     MOVE     WK-TIME-HM        TO        NFT-F02.
* 取込担当者部門ＣＤ
     MOVE     LINK-IN-BUMON     TO        NFT-F03.
* 取込担当者ＣＤ
     MOVE     LINK-IN-TANCD     TO        NFT-F04.
* 店舗ＣＤ　　
     MOVE     CSV-F01           TO        NFT-F051.
* 商品分類ＣＤ
     MOVE     CSV-F02           TO        NFT-F052.
* パターンＣＤ
     MOVE     CSV-F03           TO        NFT-F053.
* エラー区分
     IF   WK-ERR-KBN    NOT =   SPACE
          MOVE   "1"            TO        NFT-F060
     END-IF.
          MOVE    ERR-KBN(01)   TO        NFT-F061.
          MOVE    ERR-KBN(02)   TO        NFT-F062.
          MOVE    ERR-KBN(03)   TO        NFT-F063.
          MOVE    ERR-KBN(04)   TO        NFT-F064.
          MOVE    ERR-KBN(05)   TO        NFT-F065.
          MOVE    ERR-KBN(06)   TO        NFT-F066.
          MOVE    ERR-KBN(07)   TO        NFT-F067.
          MOVE    ERR-KBN(08)   TO        NFT-F068.
          MOVE    ERR-KBN(09)   TO        NFT-F069.
          MOVE    ERR-KBN(10)   TO        NFT-F06A.
*
     WRITE    NFT-REC.
     ADD      1                  TO   NFT-CNT.
*
 WRITE-NFT-EXIT.
     EXIT.
****************************************************************
*    終了
****************************************************************
 END-SEC                   SECTION.
*
     MOVE     "END-SEC"     TO    S-NAME.
*
     MOVE      CSV-CNT      TO    MSG-OUT01.
     MOVE      NFT-CNT      TO    MSG-OUT02.
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
     CLOSE     NFTEPCSV
               NFTEPWK2
               NFTENMS1
               NFSHBPL1.
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