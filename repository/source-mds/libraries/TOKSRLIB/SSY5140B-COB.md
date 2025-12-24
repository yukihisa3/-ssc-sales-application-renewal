# SSY5140B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SSY5140B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　ナフコ出荷支援　　　　　　　　　　*
*    モジュール名　　　　：　作場・数量一括変更データチェック　*
*    作成日／作成者　　　：　2019/02/19 INOUE                  *
*    処理内容　　　　　　：　作場・数量一括変更ＥＸＣＥＬデータ*
*    　　　　　　　　　　　　について、整合性チェック・件数　　*
*                            カウントを行う。　　　　　　　　　*
*    変更日／作成者　　　：　                                  *
*    変更内容　　　　　　：                                   *
****************************************************************
 IDENTIFICATION              DIVISION.
 PROGRAM-ID.                 SSY5140B.
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
*作場・数量一括変更ＥＸＣＥＬデータ
     SELECT      CHGXXXF    ASSIGN    TO       DA-01-S-CHGXXXF
                             ORGANIZATION       SEQUENTIAL
                             ACCESS    MODE     SEQUENTIAL
                             FILE      STATUS   CSV-ST.
*作場・数量一括変更ＥＸＣＥＬ取込ファイル
     SELECT      CHGXXXT1    ASSIGN    TO       DA-01-VI-CHGXXXT1
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     DYNAMIC
                             RECORD    KEY      CHG-F0501
                                                CHG-F05021
                                                CHG-F05022
                                                CHG-F05023
                                                CHG-F0503
                                                CHG-F0504
                                                CHG-F0505
                                                CHG-F0506
                                                CHG-F060
                                                WITH  DUPLICATES
                             FILE      STATUS   CHG-ST.
*作場マスタ　　　　
     SELECT   SAKUBAL1  ASSIGN    TO        DA-01-VI-SAKUBAL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       SKB-F01
                        FILE  STATUS   IS   SKB-ST.
*売上伝票ファイル
     SELECT   SHTDENLA  ASSIGN    TO        DA-01-VI-SHTDENLA
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       DEN-F46   DEN-F47
                                            DEN-F01   DEN-F48
                                            DEN-F02   DEN-F04
                                            DEN-F051  DEN-F07
                                            DEN-F112  DEN-F03
                        FILE  STATUS   IS   DEN-ST.
*ナフコ基本情報ファイル
     SELECT   NFJOHOL1  ASSIGN    TO   DA-01-VI-NFJOHOL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  JOH-F01  JOH-F05
                                       JOH-F06  JOH-F07
                                       JOH-F08  JOH-F09
                        WITH  DUPLICATES
                        FILE  STATUS   IS   JOH-ST.
*条件ファイル
*    SELECT      JYOKEN1     ASSIGN    TO       DA-01-VI-JYOKEN1
*                            ORGANIZATION       INDEXED
*                            ACCESS    MODE     RANDOM
*                            RECORD    KEY      JYO-F01 JYO-F02
*                            FILE      STATUS   JYO-ST.
****************************************************************
 DATA                        DIVISION.
****************************************************************
 FILE                        SECTION.
*作場・数量一括変更ＥＸＣＥＬデータ
 FD  CHGXXXF
     LABEL       RECORD      IS        STANDARD
     BLOCK       CONTAINS    11        RECORDS.
     COPY        CHGXXX3     OF        XFDLIB
     JOINING     CSV         AS        PREFIX.
*作場・数量一括変更ＥＸＣＥＬ取込ファイル
 FD  CHGXXXT1.
     COPY        CHGXXXT1    OF        XFDLIB
     JOINING     CHG         AS        PREFIX.
*
*売上伝票データ　ＲＬ＝１０２０
 FD  SHTDENLA
                        LABEL RECORD   IS   STANDARD.
     COPY     SHTDENF   OF        XFDLIB
              JOINING   DEN  AS   PREFIX.
*
*ナフコ基本情報Ｆ
 FD  NFJOHOL1           LABEL RECORD   IS   STANDARD.
     COPY     NFJOHOL1  OF        XFDLIB
              JOINING   JOH       PREFIX.
*
*作場マスタ
 FD  SAKUBAL1           LABEL RECORD   IS   STANDARD.
     COPY     SAKUBAL1  OF        XFDLIB
              JOINING   SKB       PREFIX.
*
*条件ファイル
*FD  JYOKEN1.
*    COPY        JYOKEN1     OF        XFDLIB
*    JOINING     JYO         AS        PREFIX.
****************************************************************
 WORKING-STORAGE           SECTION.
****************************************************************
 01  ST-AREA.
     03  CSV-ST              PIC  X(02)  VALUE  SPACE.
     03  CHG-ST              PIC  X(02)  VALUE  SPACE.
     03  DEN-ST              PIC  X(02)  VALUE  SPACE.
     03  JOH-ST              PIC  X(02)  VALUE  SPACE.
     03  SKB-ST              PIC  X(02)  VALUE  SPACE.
*    03  JYO-ST              PIC  X(02)  VALUE  SPACE.
 01  WK-AREA.
     03  END-FLG             PIC  X(03)  VALUE  SPACE.
     03  CSV-ENDFLG          PIC  X(01)  VALUE  SPACE.
     03  CSV-CNT             PIC  9(07)  VALUE  ZERO.
     03  CHG-CNT             PIC  9(07)  VALUE  ZERO.
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
 01  SAKUBAL1-INV-FLG        PIC  X(03)  VALUE  SPACE.
 01  NFJOHOL1-INV-FLG        PIC  X(03)  VALUE  SPACE.
 01  SHTDENLA-INV-FLG        PIC  X(03)  VALUE  SPACE.
*01  JYO-INV-FLG             PIC  X(03)  VALUE  SPACE.
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
     03  CHG-ERR             PIC  N(10)  VALUE
                   NC"ＥＸＣＥＬ取込Ｆ異常".
     03  SKB-ERR             PIC  N(10)  VALUE
                   NC"作場マスタ異常".
     03  DEN-ERR             PIC  N(10)  VALUE
                   NC"売上伝票ファイル異常".
     03  JOH-ERR             PIC  N(10)  VALUE
                   NC"ナフコ基本情報Ｆ異常".
*    03  JYO-ERR             PIC  N(10)  VALUE
*                  NC"条件ファイル異常".
*メッセージ出力領域
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  ST-PG           PIC  X(08)  VALUE "SSY5140B".
         05  FILLER          PIC  X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  END-PG          PIC  X(08)  VALUE "SSY5140B".
         05  FILLER          PIC  X(11)  VALUE
                                         " END   *** ".
     03  MSG-OUT1.
         05  MSG-OUT1-FIL1   PIC  X(02)  VALUE "##".
         05  MSG-OUT1-FIL2   PIC  N(13)  VALUE
                             NC"一括変更データ　　読込　＝".
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
     USE         AFTER       EXCEPTION PROCEDURE CHGXXXF.
     DISPLAY     CSV-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     CSV-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 CHG-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE CHGXXXT1.
     DISPLAY     CHG-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     CHG-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 DEN-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE   SHTDENLA.
     DISPLAY     DEN-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     DEN-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP      RUN.
 JOH-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE   NFJOHOL1.
     DISPLAY     JOH-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     JOH-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP      RUN.
 SKB-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE   SAKUBAL1.
     DISPLAY     SKB-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     SKB-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP      RUN.
*JYO-ERR                     SECTION.
*    USE         AFTER       EXCEPTION PROCEDURE JYOKEN1.
*    DISPLAY     JYO-ERR     UPON      CONS.
*    DISPLAY     SEC-NAME    UPON      CONS.
*    DISPLAY     JYO-ST      UPON      CONS.
*    MOVE        4000        TO        PROGRAM-STATUS.
*    STOP        RUN.
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
     OPEN        INPUT       CHGXXXF.
     OPEN        I-O         CHGXXXT1.
     OPEN        INPUT       SAKUBAL1 SHTDENLA NFJOHOL1.
*    OPEN        INPUT       JYOKEN1.
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
*JYO-READ-SEC               SECTION.
*    MOVE     "JYO-READ-SEC" TO   S-NAME.
*
*    READ     JYOKEN1
*      INVALID
*             MOVE "INV"     TO   JYO-INV-FLG
*      NOT  INVALID
*             MOVE SPACE     TO   JYO-INV-FLG
*    END-READ.
*JYO-READ-EXIT.
*    EXIT.
****************************************************************
*                 M A I N - S E C
****************************************************************
 MAIN-SEC                    SECTION.
     MOVE     "MAIN-SEC"          TO   S-NAME.
*
 MAIN-100.
* 作場・数量一括変更ＥＸＣＥＬ読込　終了するまで繰りかえす。
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
* 作場・数量一括変更ＥＸＣＥＬ順読込
****************************************************************
 READ-CSV-SEC                SECTION.
     MOVE     "READ-CSV-SEC"      TO   S-NAME.
*
     READ     CHGXXXF   AT   END
              MOVE      "Y"       TO   CSV-ENDFLG
              GO                  TO   READ-CSV-EXIT
     END-READ.
*カウント（取込件数）
     ADD      1                   TO   CSV-CNT.
*
 READ-CSV-EXIT.
     EXIT.
***************************************************************
*作場・数量一括変更ＥＸＣＥＬチェック編集
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
*作場・数量一括変更ＥＸＣＥＬ取込ファイル出力
     PERFORM   CHG-WRITE-SEC.
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
*【作場ＣＤ未登録チェック】
*------------------------------------------*
     MOVE     CSV-M25        TO   SKB-F01.
     PERFORM  SAKUBAL1-READ-SEC.
     IF       SAKUBAL1-INV-FLG    =    "INV"
              MOVE   "Y"     TO   WK-ERR-KBN
              MOVE   "1"     TO   ERR-KBN(01)
     END-IF.
*------------------------------------------*
 DATA-CHK-02.
*【売上伝票ファイル未登録チェック】
*------------------------------------------*
     MOVE     CSV-M02(1:8)   TO   DEN-F46.
     MOVE     CSV-M03(1:4)   TO   DEN-F47
     MOVE     CSV-M04        TO   DEN-F01.
     MOVE     CSV-M24        TO   DEN-F48.
     MOVE     CSV-M06        TO   DEN-F02.
     MOVE     0              TO   DEN-F04.
     MOVE     40             TO   DEN-F051.
     MOVE     CSV-M16        TO   DEN-F07.
     MOVE     CSV-M05(1:8)   TO   DEN-F112.
     MOVE     CSV-M07        TO   DEN-F03.
     PERFORM  SHTDENLA-READ-SEC.
     IF       SHTDENLA-INV-FLG    =    "INV"
              MOVE   "Y"     TO   WK-ERR-KBN
              MOVE   "1"     TO   ERR-KBN(03)
     END-IF.
*------------------------------------------*
 DATA-CHK-03.
*【ナフコ基本情報ファイル未登録チェック】
*------------------------------------------*
     MOVE     CSV-M01        TO   JOH-F01.
     MOVE     CSV-M24        TO   JOH-F05
     MOVE     CSV-M16        TO   JOH-F06.
     MOVE     CSV-M06        TO   JOH-F07.
     MOVE     CSV-M07        TO   JOH-F08.
     MOVE     CSV-M05(1:8)   TO   JOH-F09.
     PERFORM  NFJOHOL1-READ-SEC.
     IF       NFJOHOL1-INV-FLG    =    "INV"
              MOVE   "Y"     TO   WK-ERR-KBN
              MOVE   "1"     TO   ERR-KBN(04)
     END-IF.
*------------------------------------------*
 DATA-CHK-04.
*【整合性チェック】
*------------------------------------------*
     MOVE     CSV-M01        TO   CHG-F0501.
     MOVE     CSV-M02(1:8)   TO   CHG-F05021.
     MOVE     CSV-M03(1:4)   TO   CHG-F05022.
     MOVE     CSV-M04        TO   CHG-F05023.
     MOVE     CSV-M05        TO   CHG-F0503.
     MOVE     CSV-M16        TO   CHG-F0504.
     MOVE     CSV-M06        TO   CHG-F0505.
     MOVE     CSV-M07        TO   CHG-F0506.
     MOVE     SPACE          TO   CHG-F060.
*             DISPLAY   "CHG-F0501 =" CHG-F0501  UPON CONS.
*                       "CHG-F05021=" CHG-F05021 UPON CONS.
*                       "CHG-F05022=" CHG-F05022 UPON CONS.
*                       "CHG-F05023=" CHG-F05023 UPON CONS.
*                       "CHG-F0503=" CHG-F0503 UPON CONS.
*                       "CHG-F0504=" CHG-F0504 UPON CONS.
*                       "CHG-F0505=" CHG-F0505 UPON CONS.
*                       "CHG-F0506=" CHG-F0506 UPON CONS.
*                       "CHG-F060  =" CHG-F060   UPON CONS.
*                                                UPON CONS.
     START    CHGXXXT1   KEY  IS  >=   CHG-F0501
                                       CHG-F05021
                                       CHG-F05022
                                       CHG-F05023
                                       CHG-F0503
                                       CHG-F0504
                                       CHG-F0505
                                       CHG-F0506
                                       CHG-F060
       INVALID
              GO        TO    END-SEC-02
     END-START.
*
 READ-SEC-02.
     READ     CHGXXXT1   NEXT     AT   END
              GO        TO    END-SEC-02
     END-READ.
*
 UPDATA-SEC-02.
*
     IF    (  CHG-F0501     =    CSV-M01 ) AND
           (  CHG-F05021    =    CSV-M02(1:8) ) AND
           (  CHG-F05022    =    CSV-M03(1:4) ) AND
           (  CHG-F05023    =    CSV-M04 ) AND
           (  CHG-F0503     =    CSV-M05 ) AND
           (  CHG-F0504     =    CSV-M16 ) AND
           (  CHG-F0505     =    CSV-M06 ) AND
           (  CHG-F0506     =    CSV-M07 )
              MOVE   "Y"          TO        WK-ERR-KBN
              MOVE   "1"          TO        ERR-KBN(02)
              IF         CHG-F060  NOT = "1"
                         ADD       1     TO        ERR-CNT
                         SUBTRACT  1     FROM      OK-CNT
              END-IF
              MOVE      "1"        TO    CHG-F060  CHG-F062
              MOVE      SPACE      TO    CHG-F06A
              REWRITE    CHG-REC
              READ       CHGXXXT1
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
     CLOSE         CHGXXXT1.
     OPEN     I-O  CHGXXXT1.
*
*------------------------------------------*
 DATA-CHK-04.
*【カウント（エラー件数）】
*------------------------------------------*
     IF       WK-ERR-KBN  =   "Y"
              ADD    1        TO     ERR-CNT
     ELSE
              ADD    1        TO     OK-CNT
              MOVE  "1"       TO     ERR-KBN(10)
     END-IF.
*
 DATA-CHK-EXIT.
     EXIT.
*
****************************************************************
*  作場・数量一括変更ＥＸＣＥＬ取込ファイル出力
****************************************************************
 CHG-WRITE-SEC               SECTION.
*
     MOVE       "CHG-WRITE-SEC"    TO   S-NAME.
*
     MOVE     SPACE             TO        CHG-REC.
     INITIALIZE                           CHG-REC.
*
* 取込日付
     MOVE     SYS-DATE8         TO        CHG-F01.
* 取込時刻
     MOVE     WK-TIME-HM        TO        CHG-F02.
* 取込担当者部門ＣＤ
     MOVE     LINK-IN-BUMON     TO        CHG-F03.
* 取込担当者ＣＤ
     MOVE     LINK-IN-TANCD     TO        CHG-F04.
* 管理番号
     MOVE     CSV-M01           TO        CHG-F0501.
* バッチ日付
     MOVE     CSV-M02(1:8)      TO        CHG-F05021.
* バッチ時刻
     MOVE     CSV-M03(1:4)      TO        CHG-F05022.
* バッチ取引先
     MOVE     CSV-M04           TO        CHG-F05023.
* 納品日
     MOVE     CSV-M05           TO        CHG-F0503.
* 店舗ＣＤ
     MOVE     CSV-M16           TO        CHG-F0504.
* 伝票番号
     MOVE     CSV-M06           TO        CHG-F0505.
* 行番号
     MOVE     CSV-M07           TO        CHG-F0506.
* 発注数
     MOVE     CSV-M22           TO        CHG-F0507.
* 訂正数
     MOVE     CSV-M23           TO        CHG-F0508.
* 倉庫ＣＤ
     MOVE     CSV-M24           TO        CHG-F0509.
* 変更作場
     MOVE     CSV-M25           TO        CHG-F050A.
* エラー区分
     IF   WK-ERR-KBN    NOT =   SPACE
          MOVE   "1"            TO        CHG-F060
     END-IF.
     MOVE    ERR-KBN(01)   TO        CHG-F061.
     MOVE    ERR-KBN(02)   TO        CHG-F062.
     MOVE    ERR-KBN(03)   TO        CHG-F063.
     MOVE    ERR-KBN(04)   TO        CHG-F064.
     MOVE    ERR-KBN(05)   TO        CHG-F065.
     MOVE    ERR-KBN(06)   TO        CHG-F066.
     MOVE    ERR-KBN(07)   TO        CHG-F067.
     MOVE    ERR-KBN(08)   TO        CHG-F068.
     MOVE    ERR-KBN(09)   TO        CHG-F069.
     MOVE    ERR-KBN(10)   TO        CHG-F06A.
*
     WRITE    CHG-REC.
     ADD      1                  TO   CHG-CNT.
*
 WRITE-CHG-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    作場マスタ　　　　読込
*--------------------------------------------------------------*
 SAKUBAL1-READ-SEC       SECTION.
     MOVE    "SAKUBAL1-READ-SEC"   TO   S-NAME.
*
     READ  SAKUBAL1     INVALID
                        MOVE "INV" TO  SAKUBAL1-INV-FLG
                   NOT  INVALID
                        MOVE SPACE TO  SAKUBAL1-INV-FLG
     END-READ.
*
 SAKUBAL1-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    ナフコ基本情報Ｆ　読込
*--------------------------------------------------------------*
 NFJOHOL1-READ-SEC       SECTION.
     MOVE    "NFJOHOL1-READ-SEC"   TO   S-NAME.
*
     READ  NFJOHOL1     INVALID
                        MOVE "INV" TO  NFJOHOL1-INV-FLG
                   NOT  INVALID
                        MOVE SPACE TO  NFJOHOL1-INV-FLG
     END-READ.
*
 NFJOHOL1-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    売上伝票Ｆ　　　　読込
*--------------------------------------------------------------*
 SHTDENLA-READ-SEC      SECTION.
     MOVE    "SHTDENLA-READ-SEC"  TO   S-NAME.
*
     READ  SHTDENLA     INVALID
                        MOVE "INV" TO  SHTDENLA-INV-FLG
                   NOT  INVALID
                        MOVE SPACE TO  SHTDENLA-INV-FLG
     END-READ.
*
 SHTDENLA-READ-EXIT.
     EXIT.
****************************************************************
*    終了
****************************************************************
 END-SEC                   SECTION.
*
     MOVE     "END-SEC"     TO    S-NAME.
*
     MOVE      CSV-CNT      TO    MSG-OUT01.
     MOVE      CHG-CNT      TO    MSG-OUT02.
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
     CLOSE     CHGXXXF
               CHGXXXT1
               SAKUBAL1 SHTDENLA NFJOHOL1.
*              JYOKEN1.
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

```
