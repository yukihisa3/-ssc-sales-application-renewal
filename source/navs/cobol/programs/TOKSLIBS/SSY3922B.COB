****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　在庫ＥＸＣＥＬ連携　　　　　　　　*
*    モジュール名　　　　：　ナフコ商品分類パターンマスタ　　　*
*    　　　　　　　　　　：　更新処理　　　　　　　　　　　　　*
*    作成日／作成者　　　：　2016/08/22 MIURA                  *
*    処理内容　　　　　　：　取込チェックにてＯＫとなった　　　*
*    　　　　　　　　　　　　データより、ナブコ商品分類パタ　　*
*                            ーンマスタの作成を行う。　　　　　*
*    変更日／作成者　　　：　                                  *
*    変更内容　　　　　　：　                                  *
****************************************************************
 IDENTIFICATION              DIVISION.
 PROGRAM-ID.                 SSY3922B.
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
*商品分類パターン取込ファイル
     SELECT      NFSHBWK1    ASSIGN    TO       DA-01-VI-NFSHBWK1
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     SEQUENTIAL
                             RECORD    KEY      EXL-F060
                                                EXL-F051
                                                EXL-F053
                             FILE      STATUS   EXL-ST.
*商品分類パターンマスタ
     SELECT      NFSHBPF    ASSIGN    TO       DA-01-VS-NFSHBPF
                             ORGANIZATION       SEQUENTIAL
                             ACCESS    MODE     SEQUENTIAL
                             FILE      STATUS   SHB-ST.
****************************************************************
 DATA                        DIVISION.
****************************************************************
 FILE                        SECTION.
*商品分類パターン取込ファイル
 FD  NFSHBWK1.
     COPY        NFSHBWK1    OF        XFDLIB
     JOINING     EXL         AS        PREFIX.
*商品分類パターンマスタ
 FD  NFSHBPF.
     COPY        NFSHBPF    OF        XFDLIB
     JOINING     SHB         AS        PREFIX.
****************************************************************
 WORKING-STORAGE           SECTION.
****************************************************************
 01  ST-AREA.
     03  EXL-ST              PIC  X(02)  VALUE  SPACE.
     03  SHB-ST              PIC  X(02)  VALUE  SPACE.
 01  SV-AREA.
     03  BR-EXL-F051         PIC  X(08)  VALUE  SPACE.
     03  BR-EXL-F052         PIC  N(15)  VALUE  SPACE.
 01  WK-AREA.
     03  END-FLG             PIC  X(03)  VALUE  SPACE.
     03  SHB-FLG             PIC  9(01)  VALUE  ZERO.
     03  EXL-ENDFLG          PIC  X(01)  VALUE  SPACE.
     03  EXL-RD-CNT          PIC  9(07)  VALUE  ZERO.
     03  SHB-WT-CNT          PIC  9(07)  VALUE  ZERO.
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
     03  EXL-ERR            PIC  N(10)  VALUE
                   NC"ＥＸＣＥＬ取込Ｆ異常".
     03  SHB-ERR             PIC  N(10)  VALUE
                   NC"商品分類パターン異常".
*
*メッセージ出力領域
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  ST-PG           PIC  X(08)  VALUE "SSY3922B".
         05  FILLER          PIC  X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  END-PG          PIC  X(08)  VALUE "SSY3922B".
         05  FILLER          PIC  X(11)  VALUE
                                         " END   *** ".
     03  MSG-OUT1.
         05  MSG-OUT1-FIL1   PIC  X(02)  VALUE "##".
         05  MSG-OUT1-FIL2   PIC  N(11)  VALUE
                             NC"ＥＸＣＥＬ読込　＝".
         05  MSG-OUT01       PIC  ZZZ,ZZ9.
         05  MSG-OUT1-FIL3   PIC  X(01)  VALUE " ".
         05  MSG-OUT1-FIL4   PIC  N(01)  VALUE NC"件".
     03  MSG-OUT2.
         05  MSG-OUT2-FIL1   PIC  X(02)  VALUE "##".
         05  MSG-OUT2-FIL2   PIC  N(11)  VALUE
                             NC"商品分類パターン作成＝".
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
     USE         AFTER       EXCEPTION PROCEDURE NFSHBWK1.
     DISPLAY     EXL-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     EXL-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 SHB-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE NFSHBPF.
     DISPLAY     SHB-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     SHB-ST      UPON      CONS.
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
     OPEN        INPUT       NFSHBWK1.
     OPEN        OUTPUT      NFSHBPF.
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
* 商品分類パターンＥＸＣＥＬ読込　終了するまで繰り返す。
     PERFORM  READ-EXL-SEC.
*
     IF       EXL-ENDFLG  =   SPACE
              PERFORM         HENSYU-EXL-SEC
              GO          TO  MAIN-100
     END-IF.
*商品分類パターンマスタ作成
     IF  EXL-RD-CNT       >   ZERO
         PERFORM  SHB-NEW-SEC
     END-IF.
     MOVE    "END"        TO  END-FLG.
*
 MAIN-SEC-EXIT.
     EXIT.
****************************************************************
*  商品分類パターンＥＸＣＥＬ　順読込
****************************************************************
 READ-EXL-SEC                SECTION.
     MOVE     "READ-EXL-SEC"      TO   S-NAME.
*
     READ     NFSHBWK1   AT   END
              MOVE      "Y"       TO   EXL-ENDFLG
              GO                  TO   READ-EXL-EXIT
     END-READ.
     IF    EXL-F060  =  "1"
              MOVE      "Y"       TO   EXL-ENDFLG
              GO                  TO   READ-EXL-EXIT
     END-IF.
*カウント（取込件数）
     ADD      1                   TO   EXL-RD-CNT.
*
 READ-EXL-EXIT.
     EXIT.
***************************************************************
*    商品分類パターンＥＸＣＥＬチェック編集
***************************************************************
 HENSYU-EXL-SEC             SECTION.
*
     MOVE    "HENSYU-EXL-SEC"      TO   S-NAME.
*商品分類パターンマスタ作成
     PERFORM  SHB-NEW-SEC.
*
 HENSYU-EXL-EXIT.
     EXIT.
****************************************************************
*   商品分類パターンマスタ作成　　　　　　　　　　　　　　　　 *
****************************************************************
 SHB-NEW-SEC        SECTION.
*
     MOVE "SHB-NEW-SEC"            TO   S-NAME.
     INITIALIZE                        SHB-REC.
*
 SHB-NEW-01.
*初回分類ＣＤ
     IF    BR-EXL-F051  =  SPACE
*         商品分類ＣＤ
           MOVE  EXL-F051          TO   SHB-F01
*         商品分類名
           MOVE  EXL-F052          TO   SHB-F02
*         パターンＣＤ
           MOVE  EXL-F053          TO   SHB-F03
*         取込担当者部門ＣＤ
           MOVE  LINK-IN-BUMON     TO   SHB-F96
*         取込担当者ＣＤ
           MOVE  LINK-IN-TANCD     TO   SHB-F97
*         取込日付　
           MOVE  SYS-DATE8         TO   SHB-F98
*         取込時刻　　
           MOVE  WK-TIME-HM        TO   SHB-F99
           WRITE      SHB-REC
           ADD        1            TO   SHB-WT-CNT
           MOVE  EXL-F051          TO   BR-EXL-F051
           MOVE  EXL-F052          TO   BR-EXL-F052
           GO          TO  SHB-NEW-EXIT
     END-IF.
*
 SHB-NEW-02.
*商品分類ＣＤブレイク時
*
     IF   EXL-F051    NOT =  BR-EXL-F051   OR
          EXL-ENDFLG = "Y"
*         商品分類ＣＤ
           MOVE  BR-EXL-F051       TO   SHB-F01
*         商品分類名
           MOVE  BR-EXL-F052       TO   SHB-F02
*         パターンＣＤ
           MOVE  SPACE             TO   SHB-F03
*         取込担当者部門ＣＤ
           MOVE  LINK-IN-BUMON     TO   SHB-F96
*         取込担当者ＣＤ
           MOVE  LINK-IN-TANCD     TO   SHB-F97
*         取込日付　
           MOVE  SYS-DATE8         TO   SHB-F98
*         取込時刻　　
           MOVE  WK-TIME-HM        TO   SHB-F99
           WRITE      SHB-REC
           ADD        1            TO   SHB-WT-CNT
           MOVE  SPACE             TO   BR-EXL-F051
           MOVE  SPACE             TO   BR-EXL-F052
           IF   EXL-ENDFLG = SPACE
                GO         TO  SHB-NEW-01
           ELSE
                GO         TO  SHB-NEW-EXIT
           END-IF
     ELSE
*         商品分類ＣＤ
           MOVE  EXL-F051          TO   SHB-F01
*         商品分類名
           MOVE  EXL-F052          TO   SHB-F02
*         パターンＣＤ
           MOVE  EXL-F053          TO   SHB-F03
*         取込担当者部門ＣＤ
           MOVE  LINK-IN-BUMON     TO   SHB-F96
*         取込担当者ＣＤ
           MOVE  LINK-IN-TANCD     TO   SHB-F97
*         取込日付　
           MOVE  SYS-DATE8         TO   SHB-F98
*         取込時刻　　
           MOVE  WK-TIME-HM        TO   SHB-F99
           WRITE      SHB-REC
           ADD        1            TO   SHB-WT-CNT
     END-IF.
*
 SHB-NEW-EXIT.
     EXIT.
****************************************************************
*    終了
****************************************************************
 END-SEC                   SECTION.
*
     MOVE     "END-SEC"     TO    S-NAME.
*
     MOVE      EXL-RD-CNT   TO    MSG-OUT01.
     MOVE      SHB-WT-CNT   TO    MSG-OUT02.
     DISPLAY   MSG-OUT1-FIL1 MSG-OUT1-FIL2 MSG-OUT01
               MSG-OUT1-FIL3 MSG-OUT1-FIL4 UPON CONS.
     DISPLAY   MSG-OUT2-FIL1 MSG-OUT2-FIL2 MSG-OUT02
               MSG-OUT2-FIL3 MSG-OUT2-FIL4 UPON CONS.
*
     CLOSE     NFSHBWK1
               NFSHBPF.
*
*ＯＵＴパラメタセット
* 取込件数
*    MOVE      EXL-RD-CNT      TO    LINK-OUT-CNT1.
* 登録件数
*    MOVE      SHB-WT-CNT      TO    LINK-OUT-CNT2.
* エラー件数
     DISPLAY   MSG-END      UPON  CONS.
*
 END-EXIT.
     EXIT.
