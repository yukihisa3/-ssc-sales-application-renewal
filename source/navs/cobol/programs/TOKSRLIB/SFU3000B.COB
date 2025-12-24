****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　社内振替　　　　　　　　　　　　　*
*    モジュール名　　　　：　社内振替発注ＥＸＣＥＬチェック    *
*    作成日／作成者　　　：　2016/12/22 INOUE                  *
*    処理内容　　　　　　：　社内振替発注ＥＸＣＥＬデータ　　  *
*    　　　　　　　　　　　　について、整合性チェック・件数　　*
*                            カウントを行う。　　　　　　　　　*
*    流用　　　　　　　　：　SFU3000B                          *
*    変更日／作成者　　　：　2017/04/11 TAKAHASHI              *
*    変更内容　　　　　　：　チェック方法を変更１              *
*    変更日／作成者　　　：　2017/04/13 TAKAHASHI              *
*    変更内容　　　　　　：　チェック方法を変更２              *
****************************************************************
 IDENTIFICATION              DIVISION.
 PROGRAM-ID.                 SFU3000B.
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
*社内振替ＥＸＣＥＬデータ
     SELECT      SFRWKCSV    ASSIGN    TO       DA-01-S-SFRWKCSV
                             ORGANIZATION       SEQUENTIAL
                             ACCESS    MODE     SEQUENTIAL
                             FILE      STATUS   CSV-ST.
*社内振替ＥＸＣＥＬ取込ファイル
     SELECT      SFRWKPF     ASSIGN    TO       DA-01-VS-SFRWKPF
                             ORGANIZATION       SEQUENTIAL
                             ACCESS    MODE     SEQUENTIAL
                             FILE      STATUS   SFR-ST.
*倉庫マスタ
     SELECT      ZSOKMS1     ASSIGN    TO       DA-01-VI-ZSOKMS1
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     RANDOM
                             RECORD    KEY      SOK-F01
                             FILE      STATUS   SOK-ST.
*商品名称マスタ
     SELECT      MEIMS1      ASSIGN    TO       DA-01-VI-MEIMS1
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     RANDOM
                             RECORD    KEY      SHO-F011
                                                SHO-F0121
                                                SHO-F0122
                                                SHO-F0123
                             FILE      STATUS   SHO-ST.
*仕入先マスタ
     SELECT      ZSHIMS1     ASSIGN    TO       DA-01-VI-ZSHIMS1
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     RANDOM
                             RECORD    KEY      SHI-F01
                             FILE      STATUS   SHI-ST.
*条件ファイル
     SELECT      JYOKEN1     ASSIGN    TO       DA-01-VI-JYOKEN1
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     RANDOM
                             RECORD    KEY      JYO-F01
                                                JYO-F02
                             FILE      STATUS   JYO-ST.
*シーズンマスタ
     SELECT      SEASONL1    ASSIGN    TO       DA-01-VI-SEASONL1
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     RANDOM
                             RECORD    KEY      SEA-F01
                             FILE      STATUS   SEA-ST.
*社内振替情報ファイル
     SELECT      SFRHEDL1    ASSIGN    TO       DA-01-VI-SFRHEDL1
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     RANDOM
                             RECORD    KEY      HED-F01
                                                HED-F02
                                                HED-F03
                                                HED-F04
                                                HED-F05
                                                HED-F06
                                                HED-F07
                                                HED-F08
                             FILE      STATUS   HED-ST.
*社内振替情報明細ファイル
     SELECT      SFRMEIL1    ASSIGN    TO       DA-01-VI-SFRMEIL1
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     RANDOM
                             RECORD    KEY      MEI-F01
                                                MEI-F02
                                                MEI-F03
                                                MEI-F04
                                                MEI-F05
                                                MEI-F06
                                                MEI-F07
                                                MEI-F08
                                                MEI-F11
                                                MEI-F12
                             FILE      STATUS   MEI-ST.
****************************************************************
 DATA                        DIVISION.
****************************************************************
 FILE                        SECTION.
*社内振替ＥＸＣＥＬデータ
 FD  SFRWKCSV
     LABEL       RECORD      IS        STANDARD
     BLOCK       CONTAINS    19        RECORDS.
     COPY        SFRWKCSV    OF        XFDLIB
     JOINING     CSV         AS        PREFIX.
*社内振替ＥＸＣＥＬ取込ファイル
 FD  SFRWKPF.
     COPY        SFRWKPF     OF        XFDLIB
     JOINING     SFR         AS        PREFIX.
*倉庫マスタ
 FD  ZSOKMS1.
     COPY        ZSOKMS1     OF        XFDLIB
     JOINING     SOK         AS        PREFIX.
*商品名称マスタ
 FD  MEIMS1.
     COPY        MEIMS1      OF        XFDLIB
     JOINING     SHO         AS        PREFIX.
*仕入先マスタ
 FD  ZSHIMS1.
     COPY        ZSHIMS1     OF        XFDLIB
     JOINING     SHI         AS        PREFIX.
*条件ファイル
 FD  JYOKEN1.
     COPY        JYOKEN1     OF        XFDLIB
     JOINING     JYO         AS        PREFIX.
*シーズンマスタ
 FD  SEASONL1.
     COPY        SEASONL1    OF        XFDLIB
     JOINING     SEA         AS        PREFIX.
*社内振替情報ファイル
 FD  SFRHEDL1.
     COPY        SFRHEDL1    OF        XFDLIB
     JOINING     HED         AS        PREFIX.
*社内振替情報明細ファイル
 FD  SFRMEIL1.
     COPY        SFRMEIL1    OF        XFDLIB
     JOINING     MEI         AS        PREFIX.
****************************************************************
 WORKING-STORAGE           SECTION.
****************************************************************
 01  ST-AREA.
     03  CSV-ST              PIC  X(02)  VALUE  SPACE.
     03  SFR-ST              PIC  X(02)  VALUE  SPACE.
     03  SOK-ST              PIC  X(02)  VALUE  SPACE.
     03  SHO-ST              PIC  X(02)  VALUE  SPACE.
     03  SHI-ST              PIC  X(02)  VALUE  SPACE.
     03  JYO-ST              PIC  X(02)  VALUE  SPACE.
     03  SEA-ST              PIC  X(02)  VALUE  SPACE.
     03  HED-ST              PIC  X(02)  VALUE  SPACE.
     03  MEI-ST              PIC  X(02)  VALUE  SPACE.
 01  WK-AREA.
     03  END-FLG             PIC  X(03)  VALUE  SPACE.
     03  CSV-ENDFLG          PIC  X(01)  VALUE  SPACE.
     03  CSV-CNT             PIC  9(07)  VALUE  ZERO.
     03  SFR-CNT             PIC  9(07)  VALUE  ZERO.
     03  ERR-CNT             PIC  9(07)  VALUE  ZERO.
     03  UNKNOWN             PIC  9(07)  VALUE  ZERO.
     03  OK-CNT              PIC  9(07)  VALUE  ZERO.
**
     03  WK-ERR-TBL.
         05  ERR-KBN         PIC  X(01)  OCCURS  15.
     03  WK-ERR-KBN          PIC  X(01).
*
 01  WK-CSV-F03              PIC  X(04)  VALUE SPACE.
 01  WK-CSV-F03-X            REDEFINES   WK-CSV-F03.
     03  WK-CSV-F03R         PIC  9(04).
*
 01  SOK-INV-FLG             PIC  X(03)  VALUE  SPACE.
 01  SHO-INV-FLG             PIC  X(03)  VALUE  SPACE.
 01  SHI-INV-FLG             PIC  X(03)  VALUE  SPACE.
 01  JYO-INV-FLG             PIC  X(03)  VALUE  SPACE.
 01  SEA-INV-FLG             PIC  X(03)  VALUE  SPACE.
 01  HED-INV-FLG             PIC  X(03)  VALUE  SPACE.
 01  MEI-INV-FLG             PIC  X(03)  VALUE  SPACE.
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
 01  NENDO-FROM              PIC  9(04)  VALUE  ZERO.
 01  NENDO-TO                PIC  9(04)  VALUE  ZERO.
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
     03  SFR-ERR             PIC  N(10)  VALUE
                   NC"ＥＸＣＥＬ取込Ｆ異常".
     03  SOK-ERR             PIC  N(10)  VALUE
                   NC"倉庫マスタ異常".
     03  SHO-ERR             PIC  N(10)  VALUE
                   NC"商品名称マスタ異常".
     03  SHI-ERR             PIC  N(10)  VALUE
                   NC"仕入先マスタ異常".
     03  JYO-ERR             PIC  N(10)  VALUE
                   NC"条件ファイル異常".
     03  SEA-ERR             PIC  N(10)  VALUE
                   NC"シーズンマスタ異常".
     03  HED-ERR             PIC  N(10)  VALUE
                   NC"振替情報ファイル異常".
     03  MEI-ERR             PIC  N(10)  VALUE
                   NC"明細情報ファイル異常".
*メッセージ出力領域
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  ST-PG           PIC  X(08)  VALUE "SFU3000B".
         05  FILLER          PIC  X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  END-PG          PIC  X(08)  VALUE "SFU3000B".
         05  FILLER          PIC  X(11)  VALUE
                                         " END   *** ".
     03  MSG-OUT1.
         05  MSG-OUT1-FIL1   PIC  X(02)  VALUE "##".
         05  MSG-OUT1-FIL2   PIC  N(13)  VALUE
                             NC"振替ＥＸＣＥＬ読込　＝".
         05  MSG-OUT01       PIC  ZZZZ,ZZ9.
         05  MSG-OUT1-FIL3   PIC  X(01)  VALUE " ".
         05  MSG-OUT1-FIL4   PIC  N(01)  VALUE NC"件".
     03  MSG-OUT2.
         05  MSG-OUT2-FIL1   PIC  X(02)  VALUE "##".
         05  MSG-OUT2-FIL2   PIC  N(11)  VALUE
                             NC"取込ファイル作成　　＝".
         05  MSG-OUT02       PIC  ZZZZ,ZZ9.
         05  MSG-OUT2-FIL3   PIC  X(01)  VALUE " ".
         05  MSG-OUT2-FIL4   PIC  N(01)  VALUE NC"件".
     03  MSG-OUT3.
         05  MSG-OUT3-FIL1   PIC  X(02)  VALUE "##".
         05  MSG-OUT3-FIL2   PIC  N(11)  VALUE
                             NC"　内．エラーデータ　＝".
         05  MSG-OUT03       PIC  ZZZZ,ZZ9.
         05  MSG-OUT3-FIL3   PIC  X(01)  VALUE " ".
         05  MSG-OUT3-FIL4   PIC  N(01)  VALUE NC"件".
     03  MSG-OUT4.
         05  MSG-OUT4-FIL1   PIC  X(02)  VALUE "##".
         05  MSG-OUT4-FIL2   PIC  N(11)  VALUE
                             NC"　内．ＯＫデータ　　＝".
         05  MSG-OUT04       PIC  ZZZZ,ZZ9.
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
 01  LINK-IN-SOUKO               PIC  X(02).
 01  LINK-IN-DSOUKO              PIC  X(02).
 01  LINK-IN-BUMON               PIC  X(04).
 01  LINK-IN-TANCD               PIC  X(02).
 01  LINK-OUT-CNT1               PIC  9(07).
 01  LINK-OUT-CNT2               PIC  9(07).
****************************************************************
 PROCEDURE                   DIVISION  USING LINK-IN-SOUKO
                                             LINK-IN-DSOUKO
                                             LINK-IN-BUMON
                                             LINK-IN-TANCD
                                             LINK-OUT-CNT1
                                             LINK-OUT-CNT2.
****************************************************************
 DECLARATIVES.
 CSV-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE SFRWKCSV.
     DISPLAY     CSV-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     CSV-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 SFR-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE SFRWKPF.
     DISPLAY     SFR-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     SFR-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 SOK-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE ZSOKMS1.
     DISPLAY     SOK-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     SOK-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 SHO-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE MEIMS1.
     DISPLAY     SHO-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     SHO-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 SHI-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE ZSHIMS1.
     DISPLAY     SHI-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     SHI-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 JYO-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE JYOKEN1.
     DISPLAY     JYO-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     JYO-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 SEA-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE SEASONL1.
     DISPLAY     SEA-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     SEA-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 HED-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE SFRHEDL1.
     DISPLAY     HED-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     HED-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 MEI-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE SFRMEIL1.
     DISPLAY     MEI-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     MEI-ST      UPON      CONS.
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
     OPEN        INPUT       SFRWKCSV ZSOKMS1 MEIMS1
                             ZSHIMS1  JYOKEN1 SEASONL1
                             SFRHEDL1 SFRMEIL1.
     OPEN        OUTPUT      SFRWKPF.
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
*                 M A I N - S E C
****************************************************************
 MAIN-SEC                    SECTION.
     MOVE     "MAIN-SEC"          TO   S-NAME.
*
 MAIN-100.
* ＥＸＣＥＬ読込　終了するまで繰りかえす。
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
* ＥＸＣＥＬ順読込
****************************************************************
 READ-CSV-SEC                SECTION.
     MOVE     "READ-CSV-SEC"      TO   S-NAME.
*
     READ     SFRWKCSV   AT   END
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
*ＥＸＣＥＬチェック編集
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
*ＥＸＣＥＬ取込ファイル出力
     PERFORM   SFR-WRITE-SEC.
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
 DATA-CHK-01.
*【必須項目チェック】
*------------------------------------------*
*  『伝票区分』
     IF  CSV-F01  =  SPACE
         MOVE   "Y"          TO        WK-ERR-KBN
         MOVE   "1"          TO        ERR-KBN(01)
     END-IF.
*
*  『年度』
     IF ( CSV-F02 NOT NUMERIC ) OR
        ( CSV-F02 =   ZERO    )
         MOVE   "Y"          TO        WK-ERR-KBN
         MOVE   "1"          TO        ERR-KBN(01)
     END-IF.
*
*  『シーズン』
     IF  CSV-F03 =   SPACE
         MOVE   "Y"          TO        WK-ERR-KBN
         MOVE   "1"          TO        ERR-KBN(01)
     END-IF.
*
*  『倉庫』
     IF  CSV-F04 =   SPACE
         MOVE   "Y"          TO        WK-ERR-KBN
         MOVE   "1"          TO        ERR-KBN(01)
     END-IF.
*
*  『サカタ商品ＣＤ』
     IF  CSV-F05 =   SPACE
         MOVE   "Y"          TO        WK-ERR-KBN
         MOVE   "1"          TO        ERR-KBN(01)
     END-IF.
*
*  『商品名』
     IF  CSV-F10 =   SPACE
         MOVE   "Y"          TO        WK-ERR-KBN
         MOVE   "1"          TO        ERR-KBN(01)
     END-IF.
*
*  『入金予定日』
     IF ( CSV-F11 NOT NUMERIC ) OR
        ( CSV-F11 =   ZERO    )
         MOVE   "Y"          TO        WK-ERR-KBN
         MOVE   "1"          TO        ERR-KBN(01)
     END-IF.
*
*  『発注数』
     IF ( CSV-F14 NOT = " "   ) AND
        ( CSV-F14 NOT = "-"   )
         MOVE   "Y"          TO        WK-ERR-KBN
         MOVE   "1"          TO        ERR-KBN(01)
     END-IF.
*##  IF ( CSV-F15 NOT NUMERIC ) OR
*##     ( CSV-F15 =   ZERO    )
*##      MOVE   "Y"          TO        WK-ERR-KBN
*##      MOVE   "1"          TO        ERR-KBN(01)
*##  END-IF.
     IF ( CSV-F15 NOT NUMERIC )
         MOVE   "Y"          TO        WK-ERR-KBN
         MOVE   "1"          TO        ERR-KBN(01)
     END-IF.
*
*------------------------------------------*
 DATA-CHK-02.
*【振分情報ファイル存在チェック】
*------------------------------------------*
     MOVE     CSV-F01        TO   HED-F01  MEI-F01.
     MOVE     CSV-F02        TO   HED-F02  MEI-F02.
     MOVE     CSV-F03        TO   HED-F03  MEI-F03.
     MOVE     CSV-F04        TO   HED-F04  MEI-F04.
     MOVE     CSV-F05        TO   HED-F05  MEI-F05.
     MOVE     CSV-F06        TO   HED-F06  MEI-F06.
     MOVE     CSV-F07        TO   HED-F07  MEI-F07.
     MOVE     CSV-F08        TO   HED-F08  MEI-F08.
     MOVE     "1"            TO   MEI-F11.
     MOVE     SYS-DATE8      TO   MEI-F12.
     PERFORM  HED-READ-SEC.
     PERFORM  SRMEI-READ-SEC.
*##  IF       HED-INV-FLG    =    "   "
*##  AND      MEI-INV-FLG    =    "   "
*##           MOVE   "Y"     TO   WK-ERR-KBN
*##           MOVE   "1"     TO   ERR-KBN(02)
*##  END-IF.
******ヘッダ無し、明細有りの場合、整合性エラー印字
     IF       HED-INV-FLG    =    "INV"
     AND      MEI-INV-FLG    =    "   "
              MOVE   "Y"     TO   WK-ERR-KBN
              MOVE   "1"     TO   ERR-KBN(11)
     END-IF.
*------------------------------------------*
 DATA-CHK-03.
*【伝票区分　規定値チェック】
*------------------------------------------*
     MOVE     1              TO   JYO-F01.
     MOVE     CSV-F01        TO   JYO-F02.
     PERFORM  JYO-READ-SEC.
     IF       JYO-INV-FLG    =    "INV"
              MOVE   "Y"     TO   WK-ERR-KBN
              MOVE   "1"     TO   ERR-KBN(03)
     END-IF.
     IF       CSV-F01        =    "70"
              CONTINUE
     ELSE
              MOVE   "Y"     TO   WK-ERR-KBN
              MOVE   "1"     TO   ERR-KBN(03)
     END-IF.
*------------------------------------------*
 DATA-CHK-04.
*【年度　範囲チェック】
*------------------------------------------*
     COMPUTE  NENDO-FROM     =    WK-Y     -  1.
     COMPUTE  NENDO-TO       =    WK-Y     +  1.
     IF     ( CSV-F02       >=    NENDO-FROM ) AND
            ( CSV-F02       <=    NENDO-TO   )
              CONTINUE
     ELSE
              MOVE   "Y"     TO   WK-ERR-KBN
              MOVE   "1"     TO   ERR-KBN(04)
     END-IF.
*------------------------------------------*
 DATA-CHK-05.
*【シーズンマスタ未登録チェック】
*------------------------------------------*
     MOVE     CSV-F03        TO   SEA-F01.
     PERFORM  SEA-READ-SEC.
     IF       SEA-INV-FLG    =    "INV"
              MOVE   "Y"     TO   WK-ERR-KBN
              MOVE   "1"     TO   ERR-KBN(05)
     END-IF.
*------------------------------------------*
 DATA-CHK-06.
*【倉庫ＣＤ　　　チェック】
*------------------------------------------*
     IF       LINK-IN-DSOUKO =    "01"
              CONTINUE
     ELSE
              IF  CSV-F04    =    LINK-IN-SOUKO
                  CONTINUE
              ELSE
                  MOVE      "Y"   TO   WK-ERR-KBN
                  MOVE      "1"   TO   ERR-KBN(06)
                  GO              TO   DATA-CHK-07
              END-IF
     END-IF.
*
     MOVE     CSV-F04        TO   SOK-F01.
     PERFORM  SOK-READ-SEC.
     IF       SOK-INV-FLG    =    "INV"
              MOVE   "Y"     TO   WK-ERR-KBN
              MOVE   "1"     TO   ERR-KBN(06)
     END-IF.
*------------------------------------------*
 DATA-CHK-07.
*【商品名称マスタ未登録チェック】
*------------------------------------------*
     MOVE     CSV-F05        TO   SHO-F011.
     MOVE     CSV-F06        TO   SHO-F0121.
     MOVE     CSV-F07        TO   SHO-F0122.
     MOVE     CSV-F08        TO   SHO-F0123.
     PERFORM  MEI-READ-SEC.
     IF       SHO-INV-FLG    =    "INV"
              MOVE   "Y"     TO   WK-ERR-KBN
              MOVE   "1"     TO   ERR-KBN(07)
*T↓
*      DISPLAY "F011 =" CSV-F05 UPON CONS
*      DISPLAY "F0121=" CSV-F06 UPON CONS
*      DISPLAY "F0122=" CSV-F07 UPON CONS
*      DISPLAY "F0123=" CSV-F08 UPON CONS
*T↑
     END-IF.
*------------------------------------------*
 DATA-CHK-08.
*【入荷予定日　チェック】
*------------------------------------------*
* 論理チェック
     MOVE    "2"               TO      LINK-IN-KBN.
     MOVE     ZERO             TO      LINK-IN-YMD6.
     MOVE     CSV-F11          TO      LINK-IN-YMD8.
     MOVE     ZERO             TO      LINK-OUT-RET.
     MOVE     ZERO             TO      LINK-OUT-YMD.
     CALL    "SKYDTCKB"        USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     IF       LINK-OUT-RET     NOT =   ZERO
*T            DISPLAY NC"作業日論理エラー"  CSV-F06 UPON CONS
              MOVE    "Y"      TO      WK-ERR-KBN
              MOVE    "1"      TO      ERR-KBN(08)
     END-IF.
*------------------------------------------*
 DATA-CHK-09.
*【仕入先マスタ未登録チェック】
*------------------------------------------*
     MOVE     CSV-F12        TO   SHI-F01.
     PERFORM  SHI-READ-SEC.
     IF       SHI-INV-FLG    =    "INV"
              MOVE   "Y"     TO   WK-ERR-KBN
              MOVE   "1"     TO   ERR-KBN(09)
     END-IF.
*------------------------------------------*
 DATA-CHK-10.
*【振分情報ファイル　完納済チェック】
*------------------------------------------*
     IF       HED-INV-FLG    =    "   "
              IF  HED-F24        =    "1"
                  MOVE   "Y"     TO   WK-ERR-KBN
                  MOVE   "1"     TO   ERR-KBN(10)
              END-IF
     END-IF.
*
*------------------------------------------*
 DATA-CHK-11.
*【カウント（エラー件数）】
*------------------------------------------*
     IF       WK-ERR-KBN  =   "Y"
              ADD    1        TO     ERR-CNT
     ELSE
              ADD    1        TO     OK-CNT
              MOVE   "1"      TO     ERR-KBN(15)
     END-IF.
*
 DATA-CHK-EXIT.
     EXIT.
*
****************************************************************
*  ＥＸＣＥＬ取込ファイル出力
****************************************************************
 SFR-WRITE-SEC               SECTION.
*
     MOVE       "SFR-WRITE-SEC"    TO   S-NAME.
*
     MOVE     SPACE             TO        SFR-REC.
     INITIALIZE                           SFR-REC.
*
 SFR-WRITE-010.
* 取込日付
     MOVE     SYS-DATE8         TO        SFR-F01.
* 取込時刻
     MOVE     WK-TIME-HM        TO        SFR-F02.
* 取込担当者部門ＣＤ
     MOVE     LINK-IN-BUMON     TO        SFR-F03.
* 取込担当者ＣＤ
     MOVE     LINK-IN-TANCD     TO        SFR-F04.
* 伝票区分
     MOVE     CSV-F01           TO        SFR-F051.
* 年度
     MOVE     CSV-F02           TO        SFR-F052.
* シーズン
     MOVE     CSV-F03           TO        SFR-F053.
* 倉庫ＣＤ
     MOVE     CSV-F04           TO        SFR-F054.
* サカタ商品ＣＤ
     MOVE     CSV-F05           TO        SFR-F055.
* 品単１　　　
     MOVE     CSV-F06           TO        SFR-F056.
* 品単２　　　
     MOVE     CSV-F07           TO        SFR-F057.
* 品単３　　　
     MOVE     CSV-F08           TO        SFR-F058.
* ＪＡＮＣＤ　
     MOVE     CSV-F09           TO        SFR-F059.
* 商品名
     MOVE     CSV-F10           TO        SFR-F05A.
* 入荷予定日
     MOVE     CSV-F11           TO        SFR-F05B.
* 仕入先ＣＤ
     MOVE     CSV-F12           TO        SFR-F05C.
* 棚番
     MOVE     CSV-F13           TO        SFR-F05D.
* 発注数
     MOVE     CSV-F14           TO        SFR-F05E1.
     MOVE     CSV-F15           TO        SFR-F05E2.
* ＭＳＧ
     MOVE     CSV-F16           TO        SFR-F05F.
* エラー区分
     IF   WK-ERR-KBN    NOT =   SPACE
          MOVE   "1"            TO        SFR-F060
     END-IF.
*
     MOVE     ERR-KBN(01)       TO        SFR-F061.
     MOVE     ERR-KBN(02)       TO        SFR-F062.
     MOVE     ERR-KBN(03)       TO        SFR-F063.
     MOVE     ERR-KBN(04)       TO        SFR-F064.
     MOVE     ERR-KBN(05)       TO        SFR-F065.
     MOVE     ERR-KBN(06)       TO        SFR-F066.
     MOVE     ERR-KBN(07)       TO        SFR-F067.
     MOVE     ERR-KBN(08)       TO        SFR-F068.
     MOVE     ERR-KBN(09)       TO        SFR-F069.
     MOVE     ERR-KBN(10)       TO        SFR-F06A.
     MOVE     ERR-KBN(11)       TO        SFR-F06B.
     MOVE     ERR-KBN(12)       TO        SFR-F06C.
     MOVE     ERR-KBN(13)       TO        SFR-F06D.
     MOVE     ERR-KBN(14)       TO        SFR-F06E.
     MOVE     ERR-KBN(15)       TO        SFR-F06F.
*****DISPLAY "SFR-REC = " SFR-REC.
*
 SFR-WRITE-020.
     WRITE    SFR-REC.
     ADD      1                  TO   SFR-CNT.
*
 SFR-WRITE-EXIT.
     EXIT.
****************************************************************
*    倉庫マスタ検索
****************************************************************
 SOK-READ-SEC               SECTION.
     MOVE     "SOK-READ-SEC" TO   S-NAME.
*
     READ     ZSOKMS1
       INVALID
              MOVE "INV"     TO   SOK-INV-FLG
       NOT  INVALID
              MOVE SPACE     TO   SOK-INV-FLG
     END-READ.
 SOK-READ-EXIT.
     EXIT.
****************************************************************
*    商品名称マスタ検索
****************************************************************
 MEI-READ-SEC               SECTION.
     MOVE     "MEI-READ-SEC" TO   S-NAME.
*
     READ     MEIMS1
       INVALID
              MOVE "INV"     TO   SHO-INV-FLG
       NOT  INVALID
              MOVE SPACE     TO   SHO-INV-FLG
     END-READ.
 MEI-READ-EXIT.
     EXIT.
****************************************************************
*    仕入先マスタ検索
****************************************************************
 SHI-READ-SEC               SECTION.
     MOVE     "SHI-READ-SEC" TO   S-NAME.
*
     READ     ZSHIMS1
       INVALID
              MOVE "INV"     TO   SHI-INV-FLG
       NOT  INVALID
              MOVE SPACE     TO   SHI-INV-FLG
     END-READ.
 SHI-READ-EXIT.
     EXIT.
****************************************************************
*    条件ファイル
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
*    シーズンマスタ検索
****************************************************************
 SEA-READ-SEC               SECTION.
     MOVE     "SEA-READ-SEC" TO   S-NAME.
*
     READ     SEASONL1
       INVALID
              MOVE "INV"     TO   SEA-INV-FLG
       NOT  INVALID
              MOVE SPACE     TO   SEA-INV-FLG
     END-READ.
 SEA-READ-EXIT.
     EXIT.
****************************************************************
*    社内振替情報ファイル
****************************************************************
 HED-READ-SEC               SECTION.
     MOVE     "HED-READ-SEC" TO   S-NAME.
*
     READ     SFRHEDL1
       INVALID
              MOVE "INV"     TO   HED-INV-FLG
       NOT  INVALID
              MOVE SPACE     TO   HED-INV-FLG
     END-READ.
 HED-READ-EXIT.
     EXIT.
****************************************************************
*    社内振替情報明細ファイル
****************************************************************
 SRMEI-READ-SEC             SECTION.
     MOVE   "SRMEI-READ-SEC" TO   S-NAME.
*
     READ     SFRMEIL1
       INVALID
              MOVE "INV"     TO   MEI-INV-FLG
       NOT  INVALID
              MOVE SPACE     TO   MEI-INV-FLG
     END-READ.
 SRMEI-READ-EXIT.
     EXIT.
****************************************************************
*    終了
****************************************************************
 END-SEC                   SECTION.
*
     MOVE     "END-SEC"     TO    S-NAME.
*
     MOVE      CSV-CNT      TO    MSG-OUT01.
     MOVE      SFR-CNT      TO    MSG-OUT02.
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
     CLOSE     SFRWKCSV
               SFRWKPF
               SFRHEDL1
               ZSOKMS1
               MEIMS1
               ZSHIMS1
               JYOKEN1
               SEASONL1.
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