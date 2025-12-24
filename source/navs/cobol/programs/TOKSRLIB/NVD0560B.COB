****************************************************************
*    顧客名　　　　　　　：（株）サカタのタネ殿　　　　　　　　*
*    業務名　　　　　　　：　在庫　　　　　　　　　　　　　　　*
*    モジュール名　　　　：（ＥＸＣＥＬ取込）　　　　　　　　　*
*    　　　　　　　　　　　　倉庫間移動指示　取込チェック　　　*
*    　　　　　　　　　　　　※Ｄ３６５連携対応　　　　　　　　*
*    作成日／作成者　　　：　2022/08/30 INOUE                  *
*    処理内容　　　　　　：　倉庫間移動指示データ　　　　　　　*
*    　　　　　　　　　　　　について、整合性チェック・件数　　*
*                            カウントを行う。　　　　　　　　　*
*    変更日／作成者　　　：　                                  *
*　　変更内容　　　　　　：　                                  *
****************************************************************
 IDENTIFICATION              DIVISION.
 PROGRAM-ID.                 NVD0560B.
*    　　　　　　　流用元：　NVM0120B
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
*倉庫間移動指示ＣＳＶ
     SELECT      IDOXXXCS    ASSIGN    TO       DA-01-S-IDOXXXCS
                             ORGANIZATION       SEQUENTIAL
                             ACCESS    MODE     SEQUENTIAL
                             FILE      STATUS   CSV-ST.
*倉庫間移動取込ファイル
     SELECT      IDOXXXW1    ASSIGN    TO       DA-01-VI-IDOXXXW1
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     DYNAMIC
                             RECORD    KEY      IDO-F05  IDO-F07
                                                IDO-F08  IDO-F09
                                                IDO-F10
                                                WITH  DUPLICATES
                             FILE      STATUS   IDO-ST.
*サブ商品名称マスタ
     SELECT      SUBMEIL7    ASSIGN    TO       DA-01-VI-SUBMEIL7
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     RANDOM
                             RECORD    KEY      SBM-D01
                             FILE      STATUS   SBM-ST.
*倉庫マスタ
     SELECT      ZSOKMS1     ASSIGN    TO       DA-01-VI-ZSOKMS1
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     RANDOM
                             RECORD    KEY      SOK-F01
                             FILE      STATUS   SOK-ST.
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
*倉庫間移動指示ＣＳＶ
 FD  IDOXXXCS
     LABEL       RECORD      IS        STANDARD
     BLOCK       CONTAINS    30        RECORDS.
     COPY        IDOXXXCS    OF        XFDLIB
     JOINING     CSV         AS        PREFIX.
*倉庫間取込ファイル
 FD  IDOXXXW1.
     COPY        IDOXXXW1    OF        XFDLIB
     JOINING     IDO         AS        PREFIX.
*サブ商品名称マスタ
 FD  SUBMEIL7.
     COPY        SUBMEIL7    OF        XFDLIB
     JOINING     SBM         AS        PREFIX.
*倉庫マスタ
 FD  ZSOKMS1.
     COPY        ZSOKMS1     OF        XFDLIB
     JOINING     SOK         AS        PREFIX.
*条件ファイル
 FD  JYOKEN1.
     COPY        JYOKEN1     OF        XFDLIB
     JOINING     JYO         AS        PREFIX.
****************************************************************
 WORKING-STORAGE           SECTION.
****************************************************************
 01  ST-AREA.
     03  CSV-ST              PIC  X(02)  VALUE  SPACE.
     03  IDO-ST              PIC  X(02)  VALUE  SPACE.
     03  SOK-ST              PIC  X(02)  VALUE  SPACE.
     03  SUB-ST              PIC  X(02)  VALUE  SPACE.
     03  SBM-ST              PIC  X(02)  VALUE  SPACE.
     03  JYO-ST              PIC  X(02)  VALUE  SPACE.
 01  WK-AREA.
     03  END-FLG             PIC  X(03)  VALUE  SPACE.
     03  CSV-ENDFLG          PIC  X(01)  VALUE  SPACE.
     03  CSV-CNT             PIC  9(07)  VALUE  ZERO.
     03  IDO-CNT             PIC  9(07)  VALUE  ZERO.
     03  ERR-CNT             PIC  9(07)  VALUE  ZERO.
     03  UNKNOWN             PIC  9(07)  VALUE  ZERO.
     03  OK-CNT              PIC  9(07)  VALUE  ZERO.
     03  WK-SIME             PIC  9(08)  VALUE  ZERO.
     03  CAL-UCHIWAKE        PIC  9(08)  VALUE  ZERO.
**
     03  WK-ERR-TBL.
         05  ERR-KBN         PIC  X(01)  OCCURS  10.
     03  WK-ERR-KBN          PIC  X(01).
*
 01  WK-CSV-F03              PIC  X(04)  VALUE SPACE.
 01  WK-CSV-F03-X            REDEFINES   WK-CSV-F03.
     03  WK-CSV-F03R         PIC  9(04).
*
 01  SOK-INV-FLG             PIC  X(03)  VALUE  SPACE.
 01  SUB-INV-FLG             PIC  X(03)  VALUE  SPACE.
 01  SBM-INV-FLG             PIC  X(03)  VALUE  SPACE.
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
*日付チェックワーク
 01  WK-SYS-DATE             PIC  9(08).
 01  WK-NYS-DATE             PIC  9(08).
 01  SYS-DATE2               PIC  9(08).
 01  FILLER                  REDEFINES  SYS-DATE2.
     03  SYS-YYYY.
         05  SYS-YY2-1       PIC  9(02).
         05  SYS-YY2-2       PIC  9(02).
     03  SYS-MM2             PIC  9(02).
     03  SYS-DD2             PIC  9(02).
*****日付入力許容範囲（年月日）
 01  WK-HANI-ARE.
     03  WK-HANI1            PIC  9(08)  VALUE  ZERO.
     03  WK-HANI2            PIC  9(08)  VALUE  ZERO.
*****日付入力許容範囲（年月日）
 01  WK-HENKAN.
     03  WK-HENKAN-1         PIC  9(04)  VALUE  ZERO.
     03  WK-HENKAN-2         PIC  9(02)  VALUE  ZERO.
     03  WK-HENKAN-3         PIC  9(02)  VALUE  ZERO.
*****年チェック
 01  WK-CHKS-DATE.
     03  WK-CHKS-YYYY        PIC  9(04)  VALUE  ZERO.
     03  WK-CHKS-MM          PIC  9(02)  VALUE  ZERO.
     03  WK-CHKS-DD          PIC  9(02)  VALUE  ZERO.
*****年チェック
 01  WK-CHKN-DATE.
     03  WK-CHKN-YYYY        PIC  9(04)  VALUE  ZERO.
     03  WK-CHKN-MM          PIC  9(02)  VALUE  ZERO.
     03  WK-CHKN-DD          PIC  9(02)  VALUE  ZERO.
*****在庫締年月
 01  WK-ZAIKO-SIME           PIC  9(09).
 01  FILLER                  REDEFINES   WK-ZAIKO-SIME.
     03  WK-ZAIKO-SIME-0     PIC  9(01).
     03  WK-ZAIKO-SIME-1     PIC  9(04).
     03  WK-ZAIKO-SIME-2     PIC  9(02).
     03  WK-ZAIKO-SIME-3     PIC  9(02).
*****日付入力許容範囲（月）
 01  WK-H-TUKI.
     03  WK-H-TUKI-1         PIC  9(02)  VALUE  ZERO.
     03  WK-H-TUKI-2         PIC  9(02)  VALUE  ZERO.
 01  ZAI-SIME.
     03  ZAI-SIME1           PIC  9(06)     VALUE ZERO.
     03  ZAI-SIME1R          REDEFINES      ZAI-SIME1.
         05  ZAI-SIME1R1     PIC  9(04).
         05  ZAI-SIME1R2     PIC  9(02).
     03  ZAI-SIME2           PIC  9(02)     VALUE ZERO.
*
 01  ZAI-SIMER               REDEFINES ZAI-SIME
                             PIC  9(08).
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
                   NC"移動指示ＣＳＶ異常".
     03  IDO-ERR             PIC  N(10)  VALUE
                   NC"ＥＸＣＥＬ取込Ｆ異常".
     03  SOK-ERR             PIC  N(10)  VALUE
                   NC"倉庫マスタ異常".
     03  SUB-ERR             PIC  N(10)  VALUE
                   NC"サブ変換テーブル異常".
     03  SBM-ERR             PIC  N(10)  VALUE
                   NC"サブ名称マスタ異常".
     03  JYO-ERR             PIC  N(10)  VALUE
                   NC"条件ファイル異常".
*メッセージ出力領域
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  ST-PG           PIC  X(08)  VALUE "NVD0560B".
         05  FILLER          PIC  X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  END-PG          PIC  X(08)  VALUE "NVD0560B".
         05  FILLER          PIC  X(11)  VALUE
                                         " END   *** ".
     03  MSG-OUT1.
         05  MSG-OUT1-FIL1   PIC  X(02)  VALUE "##".
         05  MSG-OUT1-FIL2   PIC  N(11)  VALUE
                             NC"移動指示データ読込　＝".
         05  MSG-OUT01       PIC  ZZZ,ZZ9.
         05  MSG-OUT1-FIL3   PIC  X(01)  VALUE " ".
         05  MSG-OUT1-FIL4   PIC  N(01)  VALUE NC"件".
     03  MSG-OUT2.
         05  MSG-OUT2-FIL1   PIC  X(02)  VALUE "##".
         05  MSG-OUT2-FIL2   PIC  N(11)  VALUE
                             NC"取込ファイル　作成　＝".
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
     USE         AFTER       EXCEPTION PROCEDURE IDOXXXCS.
     DISPLAY     CSV-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     CSV-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 IDO-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE IDOXXXW1.
     DISPLAY     IDO-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     IDO-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 SBM-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE SUBMEIL7.
     DISPLAY     SBM-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     SBM-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 SOK-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE ZSOKMS1.
     DISPLAY     SOK-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     SOK-ST      UPON      CONS.
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
     OPEN        INPUT       IDOXXXCS
                             ZSOKMS1 SUBMEIL7.
     OPEN        I-O         IDOXXXW1.
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
     MOVE      LINK-OUT-YMD       TO   WK-DATE8 SYS-DATE2
                                       SYS-DATE8.
     DISPLAY "# HIDUKE = " WK-DATE8   UPON CONS.
     DISPLAY "# JIKAN  = " WK-TIME-HM UPON CONS.
*
*在庫締日取得
     MOVE      99            TO   JYO-F01.
     MOVE     SPACE          TO   JYO-F02.
     MOVE     "ZAI"          TO   JYO-F02.
     READ     JYOKEN1
          INVALID
              DISPLAY   "JYOKEN1 INV KEY=99"  UPON CONS
              MOVE      "END"     TO   END-FLG
              MOVE      4010      TO   PROGRAM-STATUS
              GO   TO   INIT-EXIT
     END-READ.
     MOVE     JYO-F05        TO   ZAI-SIME1.
     ADD      1              TO   ZAI-SIME1.
     IF       ZAI-SIME1R2    >    12
              MOVE     1     TO   ZAI-SIME1R2
              ADD      1     TO   ZAI-SIME1R1
     END-IF.
     MOVE     31             TO   ZAI-SIME2.
     MOVE     JYO-F04        TO   WK-ZAIKO-SIME.
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
* 倉庫間移動指示ＣＳＶ読込
* 終了するまで繰りかえす
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
* 倉庫間移動指示ＣＳＶ順読込
****************************************************************
 READ-CSV-SEC                SECTION.
     MOVE     "READ-CSV-SEC"      TO   S-NAME.
*
     READ     IDOXXXCS   AT   END
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
*倉庫間移動指示ＣＳＶチェック編集
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
*倉庫間取込ファイル出力
     PERFORM   IDO-WRITE-SEC.
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
*【移動元倉庫ＣＤ未登録チェック】
*------------------------------------------*
     MOVE     CSV-F01        TO   SOK-F01.
     PERFORM  SOK-READ-SEC.
     IF       SOK-INV-FLG    =    "INV"
              MOVE   "Y"     TO   WK-ERR-KBN
              MOVE   "1"     TO   ERR-KBN(01)
     END-IF.
*
*------------------------------------------*
 DATA-CHK-02.
*【移動先倉庫ＣＤ未登録チェック】
*------------------------------------------*
     MOVE     CSV-F03        TO   SOK-F01.
     PERFORM  SOK-READ-SEC.
     IF       SOK-INV-FLG    =    "INV"
              MOVE   "Y"     TO   WK-ERR-KBN
              MOVE   "1"     TO   ERR-KBN(02)
     END-IF.
*
*------------------------------------------*
 DATA-CHK-03.
*【移動日　常識チェック】
*------------------------------------------*
     MOVE     "2"                 TO   LINK-IN-KBN.
     MOVE     CSV-F02             TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     IF       LINK-OUT-RET   NOT =  ZERO
              MOVE   "Y"     TO   WK-ERR-KBN
              MOVE   "1"     TO   ERR-KBN(03)
     END-IF.
*
*------------------------------------------*
 DATA-CHK-04.
*【移動日　許容範囲チェック】
*------------------------------------------*
     MOVE     CSV-F02             TO   WK-NYS-DATE.
*
     MOVE     SYS-DATE2           TO   WK-CHKS-DATE.
     MOVE     WK-NYS-DATE         TO   WK-CHKN-DATE.
     IF  (WK-CHKS-YYYY        NOT =    WK-CHKN-YYYY ) AND
         (WK-CHKS-YYYY - 1    NOT =    WK-CHKN-YYYY )
              MOVE   "Y"     TO   WK-ERR-KBN
              MOVE   "1"     TO   ERR-KBN(04)
     END-IF.
*
*締日取得
     MOVE     "99"           TO   JYO-F01.
     MOVE     SPACE          TO   JYO-F02.
     PERFORM  JYO-READ-SEC.
     IF  JYO-INV-FLG    NOT =   SPACE
         DISPLAY   "JYOKEN1 INV KEY=99"  UPON STAT
         MOVE  "END"    TO   END-FLG
         MOVE  4010     TO   PROGRAM-STATUS
         GO    TO       DATA-CHK-EXIT
     END-IF.
* 入力した入出庫日の対象となる締日をワークにセットする
     EVALUATE   CSV-F02(5:2)
         WHEN      01    MOVE  JYO-F04     TO   WK-SIME
         WHEN      02    MOVE  JYO-F05     TO   WK-SIME
         WHEN      03    MOVE  JYO-F06     TO   WK-SIME
         WHEN      04    MOVE  JYO-F07     TO   WK-SIME
         WHEN      05    MOVE  JYO-F08     TO   WK-SIME
         WHEN      06    MOVE  JYO-F09     TO   WK-SIME
         WHEN      07    MOVE  JYO-F10     TO   WK-SIME
         WHEN      08    MOVE  JYO-F11     TO   WK-SIME
         WHEN      09    MOVE  JYO-F12     TO   WK-SIME
         WHEN      10    MOVE  JYO-F12A    TO   WK-SIME
         WHEN      11    MOVE  JYO-F12B    TO   WK-SIME
         WHEN      12    MOVE  JYO-F12C    TO   WK-SIME
     END-EVALUATE.
*締日－１算出
     COMPUTE   WK-SIME  =  WK-SIME  -  1.
     MOVE      SYS-DATE2          TO   WK-SYS-DATE.

*T↓
*    DISPLAY "----------------------------"   UPON  CONS.
*    DISPLAY "WK-SYS-DATE = " WK-SYS-DATE  UPON  CONS.
*    DISPLAY "WK-SIME     = " WK-SIME      UPON  CONS.
*T↑
     IF  WK-SYS-DATE   >   WK-SIME
              MOVE   "Y"     TO   WK-ERR-KBN
              MOVE   "1"     TO   ERR-KBN(04)
*T↓
*    DISPLAY "NG                         -"   UPON  CONS
*T↑
     END-IF.
*T↓
*    DISPLAY "WK-SYS-DATE = " WK-SYS-DATE  UPON  CONS.
*    DISPLAY "WK-NYS-DATE = " WK-NYS-DATE  UPON  CONS.
*T↑
     IF  WK-SYS-DATE   <   WK-NYS-DATE
              MOVE   "Y"     TO   WK-ERR-KBN
              MOVE   "1"     TO   ERR-KBN(04)
*T↓
*    DISPLAY "NG                         -"   UPON  CONS
*T↑
     END-IF.
*----<許容範囲月算出>
     MOVE    WK-ZAIKO-SIME-2  TO   WK-H-TUKI-1.
     COMPUTE WK-H-TUKI-2       =   WK-ZAIKO-SIME-2  -  1.
     IF      WK-H-TUKI-2       =   ZERO
             MOVE   12        TO   WK-H-TUKI-2
     END-IF.
*----<許容範囲開始年月日>
     EVALUATE   WK-H-TUKI-1
         WHEN      01    MOVE  JYO-F04     TO   WK-HANI1
         WHEN      02    MOVE  JYO-F05     TO   WK-HANI1
         WHEN      03    MOVE  JYO-F06     TO   WK-HANI1
         WHEN      04    MOVE  JYO-F07     TO   WK-HANI1
         WHEN      05    MOVE  JYO-F08     TO   WK-HANI1
         WHEN      06    MOVE  JYO-F09     TO   WK-HANI1
         WHEN      07    MOVE  JYO-F10     TO   WK-HANI1
         WHEN      08    MOVE  JYO-F11     TO   WK-HANI1
         WHEN      09    MOVE  JYO-F12     TO   WK-HANI1
         WHEN      10    MOVE  JYO-F12A    TO   WK-HANI1
         WHEN      11    MOVE  JYO-F12B    TO   WK-HANI1
         WHEN      12    MOVE  JYO-F12C    TO   WK-HANI1
     END-EVALUATE.
*----<許容範囲終了年月日>
     EVALUATE   WK-H-TUKI-2
         WHEN      01    MOVE  JYO-F04     TO   WK-HANI2
         WHEN      02    MOVE  JYO-F05     TO   WK-HANI2
         WHEN      03    MOVE  JYO-F06     TO   WK-HANI2
         WHEN      04    MOVE  JYO-F07     TO   WK-HANI2
         WHEN      05    MOVE  JYO-F08     TO   WK-HANI2
         WHEN      06    MOVE  JYO-F09     TO   WK-HANI2
         WHEN      07    MOVE  JYO-F10     TO   WK-HANI2
         WHEN      08    MOVE  JYO-F11     TO   WK-HANI2
         WHEN      09    MOVE  JYO-F12     TO   WK-HANI2
         WHEN      10    MOVE  JYO-F12A    TO   WK-HANI2
         WHEN      11    MOVE  JYO-F12B    TO   WK-HANI2
         WHEN      12    MOVE  JYO-F12C    TO   WK-HANI2
     END-EVALUATE.
*----<許容範囲チェック>
     MOVE          WK-HANI2    TO          WK-HENKAN.
     IF  WK-NYS-DATE   <=   WK-HANI1
         MOVE      01          TO          WK-HENKAN-3
     ELSE
         ADD       1           TO          WK-HENKAN-2
         IF        WK-HENKAN-2  >  12
                   ADD    1    TO          WK-HENKAN-1
                   MOVE  01    TO          WK-HENKAN-2
         END-IF
         MOVE      01          TO          WK-HENKAN-3
     END-IF.
     MOVE          WK-HENKAN   TO          WK-HANI2.
*T↓
*    DISPLAY "WK-HANI2    = " WK-HANI2     UPON  CONS.
*    DISPLAY "WK-NYS-DATE = " WK-NYS-DATE  UPON  CONS.
*T↑
     IF  ( WK-HANI2   <=  WK-NYS-DATE )
         CONTINUE
     ELSE
              MOVE   "Y"     TO   WK-ERR-KBN
              MOVE   "1"     TO   ERR-KBN(04)
*T↓
*    DISPLAY "NG                         -"   UPON  CONS
*    DISPLAY "----------------------------"   UPON  CONS
*T↑
     END-IF.
*
*------------------------------------------*
 DATA-CHK-06.
*【サブ商品名称マスタ存在チェック】
*------------------------------------------*
     MOVE     CSV-F04        TO   SBM-D01
     PERFORM  SBM-READ-SEC
     IF       SBM-INV-FLG     =   "INV"
              MOVE      "Y"  TO   WK-ERR-KBN
              MOVE      "1"  TO   ERR-KBN(05)
     END-IF.
*
*------------------------------------------*
 DATA-CHK-07.
*【数量チェック】
*------------------------------------------*
     IF       CSV-F07  <  CSV-F09
              MOVE      "Y"  TO   WK-ERR-KBN
              MOVE      "1"  TO   ERR-KBN(06)
     END-IF.
*
     COMPUTE  CAL-UCHIWAKE = CSV-F11 + CSV-F13 + CSV-F15 +
                             CSV-F17 + CSV-F19.
*ストック_数量内訳合計＝０以外の時、チェックを行なう
     IF  CAL-UCHIWAKE  NOT =  ZERO
         IF       CSV-F09   NOT = CAL-UCHIWAKE
                  MOVE      "Y"  TO   WK-ERR-KBN
                  MOVE      "1"  TO   ERR-KBN(06)
         END-IF
     END-IF.
*
*------------------------------------------*
 DATA-CHK-06.
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
*    サブ商品名称マスタ検索
****************************************************************
 SBM-READ-SEC               SECTION.
     MOVE    "SBM-READ-SEC" TO   S-NAME.
*
     READ     SUBMEIL7
       INVALID
              MOVE "INV"     TO   SBM-INV-FLG
       NOT  INVALID
              MOVE SPACE     TO   SBM-INV-FLG
     END-READ.
 SBM-READ-EXIT.
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
*    条件ファイル索引
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
*  倉庫間移動指示取込ファイル出力
****************************************************************
 IDO-WRITE-SEC               SECTION.
*
     MOVE    "IDO-WRITE-SEC"    TO        S-NAME.
*
     MOVE     SPACE             TO        IDO-REC.
     INITIALIZE                           IDO-REC.
*
* 取込日付
     MOVE     SYS-DATE8         TO        IDO-F01.
* 取込時刻
     MOVE     WK-TIME-HM        TO        IDO-F02.
* 取込担当者部門ＣＤ
     MOVE     LINK-IN-BUMON     TO        IDO-F03.
* 取込担当者ＣＤ
     MOVE     LINK-IN-TANCD     TO        IDO-F04.
*
* 移動元倉庫ＣＤ
     MOVE     CSV-F01           TO        IDO-F05.
* 移動日
     MOVE     CSV-F02           TO        IDO-F06.
* 移動先倉庫ＣＤ
     MOVE     CSV-F03           TO        IDO-F07.
* ＪＡＮＣＤ
     MOVE     CSV-F04           TO        IDO-F08.
* 出庫_番
     MOVE     CSV-F05           TO        IDO-F09.
* 入庫_番
     MOVE     CSV-F06           TO        IDO-F10.
* 移動指示数
     MOVE     CSV-F07           TO        IDO-F11.
* 出庫作成区分
     MOVE     CSV-F08           TO        IDO-F12.
* 出庫数
     MOVE     CSV-F09           TO        IDO-F13.
* ストックＮＯ１
     MOVE     CSV-F10           TO        IDO-F14.
*#2024/03/12 NAV ST
     IF       CSV-F10  =  "000000"
              MOVE     SPACE    TO        IDO-F14
     END-IF.
*#2024/03/12 NAV ED
* 数量内訳１
     MOVE     CSV-F11           TO        IDO-F15.
* ストックＮＯ２
     MOVE     CSV-F12           TO        IDO-F16.
*#2024/03/12 NAV ST
     IF       CSV-F12  =  "000000"
              MOVE     SPACE    TO        IDO-F16
     END-IF.
*#2024/03/12 NAV ED
* 数量内訳２
     MOVE     CSV-F13           TO        IDO-F17.
* ストックＮＯ３
     MOVE     CSV-F14           TO        IDO-F18.
*#2024/03/12 NAV ST
     IF       CSV-F14  =  "000000"
              MOVE     SPACE    TO        IDO-F18
     END-IF.
*#2024/03/12 NAV ED
* 数量内訳３
     MOVE     CSV-F15           TO        IDO-F19.
* ストックＮＯ４
     MOVE     CSV-F16           TO        IDO-F20.
*#2024/03/12 NAV ST
     IF       CSV-F16  =  "000000"
              MOVE     SPACE    TO        IDO-F20
     END-IF.
*#2024/03/12 NAV ED
* 数量内訳４
     MOVE     CSV-F17           TO        IDO-F21.
* ストックＮＯ５
     MOVE     CSV-F18           TO        IDO-F22.
*#2024/03/12 NAV ST
     IF       CSV-F18  =  "000000"
              MOVE     SPACE    TO        IDO-F22
     END-IF.
*#2024/03/12 NAV ED
* 数量内訳５
     MOVE     CSV-F19           TO        IDO-F23.
*
* 倉庫名・倉庫区分
     MOVE     CSV-F01        TO   SOK-F01.
     PERFORM  SOK-READ-SEC.
     IF   SOK-INV-FLG       =   "   "
          MOVE    SOK-F02       TO        IDO-F24
          MOVE    SOK-F14       TO        IDO-F25
     ELSE
          MOVE    ALL NC"？"    TO        IDO-F24
          MOVE    "?"           TO        IDO-F25
     END-IF.
     MOVE     CSV-F03        TO   SOK-F01.
     PERFORM  SOK-READ-SEC.
     IF   SOK-INV-FLG       =   "   "
          MOVE    SOK-F02       TO        IDO-F26
          MOVE    SOK-F14       TO        IDO-F27
     ELSE
          MOVE    ALL NC"？"    TO        IDO-F26
          MOVE    "?"           TO        IDO-F27
     END-IF.
* サカタ商品ＣＤ・品単ＣＤ
     IF   SBM-INV-FLG       =   "   "
          MOVE    SBM-F011      TO        IDO-F28
          MOVE    SBM-F012      TO        IDO-F29
     ELSE
          MOVE    ALL "?"       TO        IDO-F28
          MOVE    ALL "?"       TO        IDO-F29
     END-IF.
* 商品名１・２
     IF   SBM-INV-FLG       =   "   "
          MOVE    SBM-F021      TO        IDO-F30
          MOVE    SBM-F022      TO        IDO-F31
     ELSE
          MOVE    ALL NC"？"    TO        IDO-F30
                                          IDO-F31
     END-IF.
*
* エラー区分
     IF   WK-ERR-KBN    NOT =   SPACE
          MOVE   "1"            TO        IDO-F040
     END-IF.
     MOVE     ERR-KBN(01)       TO        IDO-F041.
     MOVE     ERR-KBN(02)       TO        IDO-F042.
     MOVE     ERR-KBN(03)       TO        IDO-F043.
     MOVE     ERR-KBN(04)       TO        IDO-F044.
     MOVE     ERR-KBN(05)       TO        IDO-F045.
     MOVE     ERR-KBN(06)       TO        IDO-F046.
     MOVE     ERR-KBN(07)       TO        IDO-F047.
     MOVE     ERR-KBN(08)       TO        IDO-F048.
     MOVE     ERR-KBN(09)       TO        IDO-F049.
     MOVE     ERR-KBN(10)       TO        IDO-F04A.
*
     WRITE    IDO-REC.
     ADD      1                  TO       IDO-CNT.
*
 WRITE-IDO-EXIT.
     EXIT.
****************************************************************
*    終了
****************************************************************
 END-SEC                   SECTION.
*
     MOVE     "END-SEC"     TO    S-NAME.
*
     MOVE      CSV-CNT      TO    MSG-OUT01.
     MOVE      IDO-CNT      TO    MSG-OUT02.
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
     CLOSE     IDOXXXCS
               IDOXXXW1
               ZSOKMS1
               SUBMEIL7
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
