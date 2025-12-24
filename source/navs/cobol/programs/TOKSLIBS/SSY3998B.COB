****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　ナフコ出荷支援　　　　　　　　　　*
*    モジュール名　　　　：　ナフコ店舗マスタ　　　　　　　　　*
*    　　　　　　　　　　：　エリアＣＤ一括置換え　　　　　　  *
*    作成日／作成者　　　：　2016/09/08 INOUE                  *
*    処理内容　　　　　　：　ナフコ店舗マスタのエリアＣＤ　　  *
*    　　　　　　　　　　　　を一括で置き換える　　　　　　　　*
*    変更日／作成者　　　：　                                  *
*    変更内容　　　　　　：　                                  *
****************************************************************
 IDENTIFICATION              DIVISION.
 PROGRAM-ID.                 SSY3998B.
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
*ナフコ店舗マスタ　エリアＣＤ一括セットデータ
     SELECT      NFTENWK     ASSIGN    TO       DA-01-VS-NFTENWK
                             ORGANIZATION       SEQUENTIAL
                             ACCESS    MODE     SEQUENTIAL
                             FILE      STATUS   EXL-ST.
*ナフコ店舗マスタ
     SELECT      NFTENMS1    ASSIGN    TO       DA-01-VI-NFTENMS1
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     RANDOM
                             RECORD    KEY      TEN-F01
                                                TEN-F02
                             FILE      STATUS   TEN-ST.
****************************************************************
 DATA                        DIVISION.
****************************************************************
 FILE                        SECTION.
*ナフコ店舗マスタ　エリアＣＤ一括セットデータ
 FD  NFTENWK.
     COPY        NFTENWK     OF        XFDLIB
     JOINING     EXL         AS        PREFIX.
*ナフコ店舗マスタ
 FD  NFTENMS1.
     COPY        NFTENMS1    OF        XFDLIB
     JOINING     TEN         AS        PREFIX.
****************************************************************
 WORKING-STORAGE           SECTION.
****************************************************************
 01  ST-AREA.
     03  EXL-ST              PIC  X(02)  VALUE  SPACE.
     03  TEN-ST              PIC  X(02)  VALUE  SPACE.
*01  SV-AREA.
*    03  BR-EXL-F051         PIC  X(08)  VALUE  SPACE.
*    03  BR-EXL-F052         PIC  N(15)  VALUE  SPACE.
 01  WK-AREA.
     03  END-FLG             PIC  X(03)  VALUE  SPACE.
     03  TEN-FLG             PIC  9(01)  VALUE  ZERO.
     03  EXL-ENDFLG          PIC  X(01)  VALUE  SPACE.
     03  EXL-RD-CNT          PIC  9(07)  VALUE  ZERO.
     03  TEN-WT-CNT          PIC  9(07)  VALUE  ZERO.
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
 01  SYS-DATE2               PIC  9(08).
 01  FILLER                  REDEFINES  SYS-DATE2.
     03  SYS-YYYY.
         05  SYS-YY2-1       PIC  9(02).
         05  SYS-YY2-2       PIC  9(02).
     03  SYS-MM2             PIC  9(02).
     03  SYS-DD2             PIC  9(02).
*
 01  SYS-TIME                PIC  9(08).
 01  WK-TIME      REDEFINES  SYS-TIME.
   03  WK-TIME-HM            PIC  9(06).
   03  WK-TIME-FIL           PIC  X(02).
 01  FILLER       REDEFINES  SYS-TIME.
     03  SYS-HH              PIC  9(02).
     03  SYS-MN              PIC  9(02).
     03  SYS-SS              PIC  9(02).
     03  FILLER              PIC  9(02).
*
***  エラーセクション名
 01  SEC-NAME.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-SEC    => ".
     03  S-NAME                   PIC  X(20).
*
 01  FILE-ERR.
     03  EXL-ERR             PIC  N(10)  VALUE
                   NC"一括セットデータ異常".
     03  TEN-ERR             PIC  N(10)  VALUE
                   NC"ナフコ店舗マスタ異常".
*
*メッセージ出力領域
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  ST-PG           PIC  X(08)  VALUE "SSY3998B".
         05  FILLER          PIC  X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  END-PG          PIC  X(08)  VALUE "SSY3998B".
         05  FILLER          PIC  X(11)  VALUE
                                         " END   *** ".
     03  MSG-OUT1.
         05  MSG-OUT1-FIL1   PIC  X(02)  VALUE "##".
         05  MSG-OUT1-FIL2   PIC  N(11)  VALUE
                             NC"一括セットＤＴ読込　＝".
         05  MSG-OUT01       PIC  ZZZ,ZZ9.
         05  MSG-OUT1-FIL3   PIC  X(01)  VALUE " ".
         05  MSG-OUT1-FIL4   PIC  N(01)  VALUE NC"件".
     03  MSG-OUT2.
         05  MSG-OUT2-FIL1   PIC  X(02)  VALUE "##".
         05  MSG-OUT2-FIL2   PIC  N(11)  VALUE
                             NC"エリアＣＤ置換え　＝".
         05  MSG-OUT02       PIC  ZZZ,ZZ9.
         05  MSG-OUT2-FIL3   PIC  X(01)  VALUE " ".
         05  MSG-OUT2-FIL4   PIC  N(01)  VALUE NC"件".
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
****************************************************************
 PROCEDURE                   DIVISION  USING LINK-IN-BUMON
                                             LINK-IN-TANCD.
****************************************************************
 DECLARATIVES.
 EXL-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE NFTENWK.
     DISPLAY     EXL-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     EXL-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 TEN-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE NFTENMS1.
     DISPLAY     TEN-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     TEN-ST      UPON      CONS.
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
     OPEN        INPUT       NFTENWK.
     OPEN        I-O         NFTENMS1.
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
     DISPLAY "# DATE  = " WK-DATE8     UPON CONS.
     DISPLAY "# TIME  = " WK-TIME-HM   UPON CONS.
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
* 一括更新データ読込　
     PERFORM  READ-EXL-SEC.
*
     IF       END-FLG     =   SPACE
              PERFORM         TEN-UPD-SEC
     END-IF.
*
 MAIN-SEC-EXIT.
     EXIT.
****************************************************************
*  一括更新データ　　　　　　　順読込
****************************************************************
 READ-EXL-SEC                SECTION.
     MOVE     "READ-EXL-SEC"      TO   S-NAME.
*
     READ     NFTENWK   AT   END
              MOVE      "END"     TO   END-FLG
              GO                  TO   READ-EXL-EXIT
     END-READ.
*カウント（取込件数）
     ADD      1                   TO   EXL-RD-CNT.
*
 READ-EXL-EXIT.
     EXIT.
****************************************************************
*   ナフコ店舗マスタ更新　　　　　　　　　　　　　　　　　　　 *
****************************************************************
 TEN-UPD-SEC        SECTION.
*
     MOVE "TEN-UPD-SEC"            TO   S-NAME.
*
 TEN-UPD-01.
*ナフコ店舗マスタ検索
     MOVE  137607                  TO   TEN-F01.
     MOVE  EXL-F01                 TO   TEN-F02.
     READ  NFTENMS1
           INVALID
             DISPLAY NC"ナフコ店舗マスタなし！？" UPON CONS
             DISPLAY NC"取引先ＣＤ＝" "137607"    UPON CONS
             DISPLAY NC"店舗ＣＤ　＝" EXL-F01     UPON CONS
             GO   TO   TEN-UPD-EXIT
     END-READ.
*
 TEN-UPD-02.
*ナフコ店舗マスタ更新
*   エリアＣＤ
     MOVE  EXL-F02           TO   TEN-F13
*   更新担当者部門ＣＤ
     MOVE  LINK-IN-BUMON     TO   TEN-F96
*   更新担当者ＣＤ
     MOVE  LINK-IN-TANCD     TO   TEN-F97
*   更新年月日
     MOVE  SYS-DATE8         TO   TEN-F98
*   取込時刻　　
     MOVE  WK-TIME-HM        TO   TEN-F99
*
     REWRITE    TEN-REC
*
     ADD        1            TO   TEN-WT-CNT
*
 TEN-UPD-EXIT.
     EXIT.
****************************************************************
*    終了
****************************************************************
 END-SEC                   SECTION.
*
     MOVE     "END-SEC"     TO    S-NAME.
*
     MOVE      EXL-RD-CNT   TO    MSG-OUT01.
     MOVE      TEN-WT-CNT   TO    MSG-OUT02.
     DISPLAY   MSG-OUT1-FIL1 MSG-OUT1-FIL2 MSG-OUT01
               MSG-OUT1-FIL3 MSG-OUT1-FIL4 UPON CONS.
     DISPLAY   MSG-OUT2-FIL1 MSG-OUT2-FIL2 MSG-OUT02
               MSG-OUT2-FIL3 MSG-OUT2-FIL4 UPON CONS.
*
     CLOSE     NFTENWK
               NFTENMS1.
*
*ＯＵＴパラメタセット
* 取込件数
*    MOVE      EXL-RD-CNT      TO    LINK-OUT-CNT1.
* 登録件数
*    MOVE      TEN-WT-CNT      TO    LINK-OUT-CNT2.
* エラー件数
     DISPLAY   MSG-END      UPON  CONS.
*
 END-EXIT.
     EXIT.
