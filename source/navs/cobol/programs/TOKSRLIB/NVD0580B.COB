****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　マスタ保守　　　　　　　　　　　　*
*    モジュール名　　　　：（ＥＸＣＥＬ取込）　　　　　　　　　*
*    　　　　　　　　　　　　Ｄ３６５入出庫ファイル更新　　　　*
*    　　　　　　　　　　　　　（倉庫間移動）　　　　　　　　　*
*    作成日／作成者　　　：　2022/09/06 INOUE                  *
*    処理内容　　　　　　：　取込チェックにてＯＫとなった　　　*
*    　　　　　　　　　　　　データより、入出庫ファイル・　　　*
*    　　　　　　　　　　　　在庫マスタ更新する　　　　　　　　*
*    変更日／変更者　　　：　                                  *
*    変更内容　　　　　　：　                                  *
****************************************************************
 IDENTIFICATION              DIVISION.
 PROGRAM-ID.                 NVD0580B.
*                       流用:NVM0140B
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
*倉庫間移動指示取込ファイル
     SELECT      IDOXXXW3    ASSIGN    TO       DA-01-VI-IDOXXXW3
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     SEQUENTIAL
                             RECORD    KEY      IDO-F05
                                                IDO-F07
                                                IDO-F12
                                                IDO-F08
                                                IDO-F09
                                                IDO-F10
                             FILE      STATUS   IDO-ST.
*Ｄ３６５入出庫Ｆ
     SELECT      DNSFILL1    ASSIGN    TO       DA-01-VI-DNSFILL1
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     DYNAMIC
                             RECORD    KEY      DNS-F01
                                                DNS-F02
                             FILE      STATUS   DNS-ST.
*サブ商品名称マスタ
     SELECT      SUBMEIL1    ASSIGN    TO       DA-01-VI-SUBMEIL1
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     RANDOM
                             RECORD    KEY      SUB-F011
                                                SUB-F012
                             FILE      STATUS   SUB-ST.
*条件ファイル
     SELECT      JYOKEN1     ASSIGN    TO       DA-01-VI-JYOKEN1
                             ORGANIZATION        INDEXED
                             ACCESS    MODE      RANDOM
                             RECORD    KEY       JYO-F01   JYO-F02
                             FILE      STATUS    JYO-ST.
*在庫マスタ
     SELECT      ZAMZAIL1    ASSIGN    TO        DA-01-VI-ZAMZAIL1
                             ORGANIZATION        INDEXED
                             ACCESS    MODE      RANDOM
                             RECORD    KEY       ZAI-F01  ZAI-F02
                                                 ZAI-F03
                             FILE      STATUS    ZAI-ST.
****************************************************************
 DATA                        DIVISION.
****************************************************************
 FILE                        SECTION.
*倉庫間移動指示取込ファイル
 FD  IDOXXXW3.
     COPY        IDOXXXW3    OF        XFDLIB
     JOINING     IDO         AS        PREFIX.
*Ｄ３６５入出庫Ｆ
 FD  DNSFILL1.
     COPY        DNSFILL1    OF        XFDLIB
     JOINING     DNS         AS        PREFIX.
*サブ商品名称マスタ
 FD  SUBMEIL1.
     COPY        SUBMEIL1    OF        XFDLIB
     JOINING     SUB         AS        PREFIX.
*条件ファイル
 FD  JYOKEN1.
     COPY        JYOKEN1     OF        XFDLIB
     JOINING     JYO         AS        PREFIX.
*在庫マスタ
 FD  ZAMZAIL1.
     COPY        ZAMZAIL1    OF        XFDLIB
     JOINING     ZAI         AS        PREFIX.
****************************************************************
 WORKING-STORAGE           SECTION.
****************************************************************
 01  ST-AREA.
     03  IDO-ST              PIC  X(02)  VALUE  SPACE.
     03  DNS-ST              PIC  X(02)  VALUE  SPACE.
     03  SUB-ST              PIC  X(02)  VALUE  SPACE.
     03  JYO-ST              PIC  X(02)  VALUE  SPACE.
     03  ZAI-ST              PIC  X(02)  VALUE  SPACE.
 01  WK-AREA.
     03  END-FLG             PIC  X(03)  VALUE  SPACE.
     03  IDO-ENDFLG          PIC  X(01)  VALUE  SPACE.
     03  IDO-RD-CNT          PIC  9(07)  VALUE  ZERO.
     03  DNS-FLG             PIC  X(03)  VALUE  SPACE.
     03  SUB-FLG             PIC  X(03)  VALUE  SPACE.
     03  INV-FLG             PIC  9(01)  VALUE  ZERO.
     03  ZAI-FLG             PIC  9(01)  VALUE  ZERO.
     03  DNS-WT-CNT          PIC  9(07)  VALUE  ZERO.
     03  DNS-RT-CNT          PIC  9(07)  VALUE  ZERO.
     03  ZAI-WT-CNT          PIC  9(07)  VALUE  ZERO.
     03  ZAI-RT-CNT          PIC  9(07)  VALUE  ZERO.
     03  SUB-WT-CNT          PIC  9(07)  VALUE  ZERO.
     03  SUB-RT-CNT          PIC  9(07)  VALUE  ZERO.
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
*在庫締日
 01  ZAI-SIME.
     03  ZAI-SIME1           PIC  9(06)     VALUE ZERO.
     03  ZAI-SIME1R          REDEFINES      ZAI-SIME1.
         05  ZAI-SIME1R1     PIC  9(04).
         05  ZAI-SIME1R2     PIC  9(02).
     03  ZAI-SIME2           PIC  9(02)     VALUE ZERO.
*
 01  ZAI-SIMER               REDEFINES ZAI-SIME
                             PIC  9(08).
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
*
*ブレイク判定ワーク
 01  BRK-IDO-F05             PIC  X(02) VALUE "  ".
 01  BRK-IDO-F07             PIC  X(02) VALUE "  ".
 01  BRK-IDO-F12             PIC  X(01) VALUE " ".
*伝票番号採番サブルーチン使用
 01  OUT-DENNO               PIC 9(07).
*行カウンタ
 01  GYO-CNT                 PIC  9(01) VALUE 7.
*
***  エラーセクション名
 01  SEC-NAME.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-SEC    => ".
     03  S-NAME                   PIC  X(20).
*
 01  FILE-ERR.
     03  IDO-ERR            PIC  N(10)  VALUE
                   NC"ＥＸＣＥＬ取込Ｆ異常".
     03  DNS-ERR             PIC  N(10)  VALUE
                   NC"Ｄ３６５入出庫Ｆ異常".
     03  SUB-ERR             PIC  N(11)  VALUE
                   NC"サブ商品名称マスタ異常".
     03  JYO-ERR             PIC  N(08)  VALUE
                   NC"条件ファイル異常".
     03  ZAI-ERR             PIC  N(07)  VALUE
                   NC"在庫マスタ異常".
*
*メッセージ出力領域
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  ST-PG           PIC  X(08)  VALUE "NVD0580B".
         05  FILLER          PIC  X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  END-PG          PIC  X(08)  VALUE "NVD0580B".
         05  FILLER          PIC  X(11)  VALUE
                                         " END   *** ".
     03  MSG-OUT1.
         05  MSG-OUT1-FIL1   PIC  X(02)  VALUE "##".
         05  MSG-OUT1-FIL2   PIC  N(11)  VALUE
                             NC"取込ファイル読込　＝".
         05  MSG-OUT01       PIC  ZZZ,ZZ9.
         05  MSG-OUT1-FIL3   PIC  X(01)  VALUE " ".
         05  MSG-OUT1-FIL4   PIC  N(01)  VALUE NC"件".
     03  MSG-OUT2.
         05  MSG-OUT2-FIL1   PIC  X(02)  VALUE "##".
         05  MSG-OUT2-FIL2   PIC  N(11)  VALUE
                             NC"入出庫ファイル作成＝".
         05  MSG-OUT02       PIC  ZZZ,ZZ9.
         05  MSG-OUT2-FIL3   PIC  X(01)  VALUE " ".
         05  MSG-OUT2-FIL4   PIC  N(01)  VALUE NC"件".
*    03  MSG-OUT3.
*        05  MSG-OUT3-FIL1   PIC  X(02)  VALUE "##".
*        05  MSG-OUT3-FIL2   PIC  N(11)  VALUE
*                            NC"変換テーブル更新＝".
*        05  MSG-OUT03       PIC  ZZZ,ZZ9.
*        05  MSG-OUT3-FIL3   PIC  X(01)  VALUE " ".
*        05  MSG-OUT3-FIL4   PIC  N(01)  VALUE NC"件".
     03  MSG-OUT4.
         05  MSG-OUT4-FIL1   PIC  X(02)  VALUE "##".
         05  MSG-OUT4-FIL2   PIC  N(11)  VALUE
                             NC"在庫マスタ　作成＝".
         05  MSG-OUT04       PIC  ZZZ,ZZ9.
         05  MSG-OUT4-FIL3   PIC  X(01)  VALUE " ".
         05  MSG-OUT4-FIL4   PIC  N(01)  VALUE NC"件".
     03  MSG-OUT5.
         05  MSG-OUT5-FIL1   PIC  X(02)  VALUE "##".
         05  MSG-OUT5-FIL2   PIC  N(11)  VALUE
                             NC"在庫マスタ　更新＝".
         05  MSG-OUT05       PIC  ZZZ,ZZ9.
         05  MSG-OUT5-FIL3   PIC  X(01)  VALUE " ".
         05  MSG-OUT5-FIL4   PIC  N(01)  VALUE NC"件".
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
 IDO-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE IDOXXXW3.
     DISPLAY     IDO-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     IDO-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 DNS-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE DNSFILL1.
     DISPLAY     DNS-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     DNS-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 SUB-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE SUBMEIL1.
     DISPLAY     SUB-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     SUB-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 JYO-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE JYOKEN1.
     DISPLAY     JYO-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     JYO-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 ZAI-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE ZAMZAIL1.
     DISPLAY     ZAI-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     ZAI-ST      UPON      CONS.
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
     OPEN        INPUT       IDOXXXW3 SUBMEIL1 JYOKEN1.
     OPEN        I-O         DNSFILL1
                             ZAMZAIL1.
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
*在庫締日取得
     MOVE      99            TO   JYO-F01.
     MOVE     SPACE          TO   JYO-F02.
     MOVE     "ZAI"          TO   JYO-F02.
     READ     JYOKEN1
          INVALID
              DISPLAY   "JYOKEN1 INV KEY=99"  UPON CONS
              MOVE      "END"     TO   END-FLG
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
 INIT-EXIT.
     EXIT.
****************************************************************
*                 M A I N - S E C
****************************************************************
 MAIN-SEC                    SECTION.
     MOVE     "MAIN-SEC"          TO   S-NAME.
*
 MAIN-100.
* 倉庫間移動指示取込ファイル読込　終了するまで繰り返す。
     PERFORM  READ-IDO-SEC.
     IF     ( IDO-ENDFLG   =  SPACE       ) AND
            ( IDO-F05      =  BRK-IDO-F05 ) AND
            ( IDO-F07      =  BRK-IDO-F07 ) AND
            ( IDO-F12      =  BRK-IDO-F12 ) AND
            ( GYO-CNT      <  7           )
              ADD      1        TO  GYO-CNT
     END-IF.
     IF    ( IDO-ENDFLG    =  SPACE       ) AND
           (( IDO-F05  NOT =  BRK-IDO-F05 ) OR
            ( IDO-F07  NOT =  BRK-IDO-F07 ) OR
            ( IDO-F12  NOT =  BRK-IDO-F12 ) OR
            ( GYO-CNT      >  6           ))
              PERFORM  DPNO-SET-RTN
              MOVE     1        TO  GYO-CNT
              MOVE     IDO-F05  TO  BRK-IDO-F05
              MOVE     IDO-F07  TO  BRK-IDO-F07
              MOVE     IDO-F12  TO  BRK-IDO-F12
     END-IF.
*
     IF       IDO-ENDFLG  =   SPACE
              PERFORM         HENSYU-IDO-SEC
              GO          TO  MAIN-100
     END-IF.
*
     MOVE    "END"        TO  END-FLG.
*
 MAIN-SEC-EXIT.
     EXIT.
****************************************************************
*  倉庫間移動指示取込ファイル　順読込
****************************************************************
 READ-IDO-SEC                SECTION.
     MOVE     "READ-IDO-SEC"      TO   S-NAME.
*
     READ     IDOXXXW3   AT   END
              MOVE      "Y"       TO   IDO-ENDFLG
              GO                  TO   READ-IDO-EXIT
     END-READ.
     IF       IDO-F040  =  "1"
              GO                  TO   READ-IDO-SEC
     END-IF.
*カウント（取込件数）
     ADD      1                   TO   IDO-RD-CNT.
*
 READ-IDO-EXIT.
     EXIT.
***************************************************************
*    作成・更新制御
***************************************************************
 HENSYU-IDO-SEC             SECTION.
*
     MOVE    "HENSYU-IDO-SEC"      TO   S-NAME.
*
*参照マスタ事前検索
     MOVE     IDO-F28      TO      SUB-F011.
     MOVE     IDO-F29      TO      SUB-F012.
     PERFORM  SUB-READ-SEC.
*作成・更新制御
*　　出庫作成区分により制御
     IF       IDO-F12  =  " "
*　　         入出庫ファイルのみ作成
              PERFORM  DNS-NEW-SEC
     ELSE
*　　         入出庫ファイル作成・在庫マスタ更新
              PERFORM  DNS-NEW-SEC
              PERFORM  ZAI-UPD-SEC
     END-IF.
*
 HENSYU-IDO-EXIT.
     EXIT.
****************************************************************
*   Ｄ３６５入出庫Ｆ作成
****************************************************************
 DNS-NEW-SEC        SECTION.
*
     MOVE "DNS-NEW-SEC"      TO   S-NAME.
     MOVE  SPACE             TO   DNS-REC.
     INITIALIZE                   DNS-REC.
*
*   伝票番号
     MOVE  OUT-DENNO         TO   DNS-F01.
*
*   行番号
     MOVE  GYO-CNT           TO   DNS-F02.
*
*   商品ＣＤ
     MOVE  IDO-F28           TO   DNS-F03.
*
*   品単ＣＤ
     MOVE  IDO-F29           TO   DNS-F04.
*
*   出庫倉庫
     MOVE  IDO-F05           TO   DNS-F05.
*
*   出庫倉庫区分
     MOVE  IDO-F25           TO   DNS-F06.
*
*   出庫_番
     MOVE  IDO-F09           TO   DNS-F07.
*
*   入庫倉庫
     MOVE  IDO-F07           TO   DNS-F08.
*
*   入庫倉庫区分
     MOVE  IDO-F27           TO   DNS-F09.
*
*   入庫_番
     MOVE  IDO-F10           TO   DNS-F10.
*
*   担当者CD
     MOVE  IDO-F04           TO   DNS-F11.
*
*   入出庫指示日
     MOVE  IDO-F06           TO   DNS-F12.
*
*   入出庫指示数量
     MOVE  IDO-F11           TO   DNS-F13.
*
*   出庫日
     IF    IDO-F12  =  "1"
           MOVE  IDO-F06     TO   DNS-F14
     END-IF.
*
*   出庫数量
     IF    IDO-F12  =  "1"
           MOVE  IDO-F13     TO   DNS-F15
     END-IF.
*   出庫担当者
     IF    IDO-F12  =  "1"
           MOVE  IDO-F04     TO   DNS-F16
     END-IF.
*   出庫完了区分
     IF    IDO-F12  =  "1"
           MOVE  "1"         TO   DNS-F17
     END-IF.
*   入庫日
     IF    IDO-F27  =  "1"
           MOVE  IDO-F06     TO   DNS-F18
     END-IF.
*   入庫数量
     IF    IDO-F27  =  "1"
           MOVE  IDO-F11     TO   DNS-F19
     END-IF.
*   入庫担当者
     IF    IDO-F27  =  "1"
           MOVE  IDO-F04     TO   DNS-F20
     END-IF.
*   入庫完了区分
     IF    IDO-F27  =  "1"
           MOVE  "1"         TO   DNS-F21
     END-IF.
*   ストックNO1
     IF    IDO-F12  =  "1"
           MOVE  IDO-F14     TO   DNS-F25(1)
     END-IF.
*   ストックNO2
     IF    IDO-F12  =  "1"
           MOVE  IDO-F16     TO   DNS-F25(2)
     END-IF.
*   ストックNO3
     IF    IDO-F12  =  "1"
           MOVE  IDO-F18     TO   DNS-F25(3)
     END-IF.
*   ストックNO4
     IF    IDO-F12  =  "1"
           MOVE  IDO-F20     TO   DNS-F25(4)
     END-IF.
*   ストックNO5
     IF    IDO-F12  =  "1"
           MOVE  IDO-F22     TO   DNS-F25(5)
     END-IF.
*   ストックNO1数量
     IF    IDO-F12  =  "1"
           MOVE  IDO-F15     TO   DNS-F28(1)
     END-IF.
*   ストックNO2数量
     IF    IDO-F12  =  "1"
           MOVE  IDO-F17     TO   DNS-F28(2)
     END-IF.
*   ストックNO3数量
     IF    IDO-F12  =  "1"
           MOVE  IDO-F19     TO   DNS-F28(3)
     END-IF.
*   ストックNO4数量
     IF    IDO-F12  =  "1"
           MOVE  IDO-F21     TO   DNS-F28(4)
     END-IF.
*   ストックNO5数量
     IF    IDO-F12  =  "1"
           MOVE  IDO-F23     TO   DNS-F28(5)
     END-IF.
*   登録日
     MOVE  SYS-DATE8         TO   DNS-F94.
*   登録時刻
     MOVE  WK-TIME-HM        TO   DNS-F95.
*   登録担当者
     MOVE  IDO-F04           TO   DNS-F96.
*
*------------------------------------------------------------
*
*   Ｄ３６５入出庫ＦＷＲＩＴＥ
     WRITE      DNS-REC.
     ADD        1            TO   DNS-WT-CNT.
*
 DNS-NEW-EXIT.
     EXIT.
****************************************************************
*    LEVEL  4     在庫マスタ更新処理　　　　　　　　　　　 *
****************************************************************
 ZAI-UPD-SEC            SECTION.
*
     MOVE "ZAI-UPD-SEC"      TO   S-NAME.
*
*----PERFORM    ZAI-OLD-RTN.
*
     PERFORM    ZAI-NEW-RTN.
*
 ZAI-UPD-EXIT.
     EXIT.
****************************************************************
*    LEVEL  ALL   商品在庫マスタ（変更前情報の更新）　　　　　 *
****************************************************************
 ZAI-OLD-RTN        SECTION.
*出庫情報
     MOVE     2                 TO   ZAI-FLG.
     MOVE     DNS-F05           TO   ZAI-F01.
     MOVE     DNS-F03           TO   ZAI-F021.
     MOVE     DNS-F04           TO   ZAI-F022.
     MOVE     DNS-F07           TO   ZAI-F03.
     PERFORM    ZAI-READ-RTN.
     IF    INV-FLG    =  1
           PERFORM   ZAI-INIT-RTN
           PERFORM   ZAI-SUBTRACT-RTN
           MOVE      SYS-DATE8    TO   ZAI-F98
           WRITE     ZAI-REC
           ADD       1          TO   ZAI-WT-CNT
     ELSE
           PERFORM   ZAI-SUBTRACT-RTN
           MOVE      SYS-DATE8    TO   ZAI-F99
           REWRITE   ZAI-REC
           ADD       1          TO   ZAI-RT-CNT
     END-IF.
*入庫情報
     MOVE     1                 TO   ZAI-FLG.
     MOVE     DNS-F08           TO   ZAI-F01.
     MOVE     DNS-F03           TO   ZAI-F021.
     MOVE     DNS-F04           TO   ZAI-F022.
     MOVE     DNS-F10           TO   ZAI-F03.
     PERFORM    ZAI-READ-RTN.
     IF    INV-FLG    =  1
           PERFORM   ZAI-INIT-RTN
           PERFORM   ZAI-SUBTRACT-RTN
           MOVE      SYS-DATE8    TO   ZAI-F98
*????????? WRITE     ZAI-REC
     ELSE
           PERFORM   ZAI-SUBTRACT-RTN
           MOVE      SYS-DATE8    TO   ZAI-F99
*????????? REWRITE   ZAI-REC
     END-IF.
 ZAI-OLD-EXIT.
     EXIT.
*
****************************************************************
*    LEVEL  ALL   商品在庫マスタ（変更後情報の更新）　　　　　 *
****************************************************************
 ZAI-NEW-RTN        SECTION.
*出庫情報
     MOVE     2                 TO   ZAI-FLG.
     MOVE     DNS-F05           TO   ZAI-F01.
     MOVE     DNS-F03           TO   ZAI-F021.
     MOVE     DNS-F04           TO   ZAI-F022.
     MOVE     DNS-F07           TO   ZAI-F03.
     PERFORM    ZAI-READ-RTN.
     IF    INV-FLG    =  1
           PERFORM   ZAI-INIT-RTN
           PERFORM   ZAI-ADD-RTN
           MOVE      SYS-DATE8    TO   ZAI-F98
           WRITE     ZAI-REC
           ADD       1            TO   ZAI-WT-CNT
     ELSE
           PERFORM   ZAI-ADD-RTN
           MOVE      SYS-DATE8    TO   ZAI-F99
           REWRITE   ZAI-REC
           ADD       1            TO   ZAI-RT-CNT
     END-IF.
*入庫情報
     MOVE  1                 TO   ZAI-FLG.
     MOVE  DNS-F08           TO   ZAI-F01.
     MOVE  DNS-F03           TO   ZAI-F021.
     MOVE  DNS-F04           TO   ZAI-F022.
     MOVE  DNS-F10           TO   ZAI-F03.
     PERFORM    ZAI-READ-RTN.
     IF    INV-FLG    =  1
           PERFORM   ZAI-INIT-RTN
           PERFORM   ZAI-ADD-RTN
           MOVE      SYS-DATE8    TO   ZAI-F98
*??????????WRITE     ZAI-REC
     ELSE
           PERFORM   ZAI-ADD-RTN
           MOVE      SYS-DATE8    TO   ZAI-F99
*??????????REWRITE   ZAI-REC
     END-IF.
 ZAI-NEW-EXIT.
     EXIT.
****************************************************************
*    LEVEL  ALL   商品在庫マスタ　ＲＥＡＤ　　　　　　　　　　 *
****************************************************************
 ZAI-READ-RTN       SECTION.
     MOVE     0         TO   INV-FLG.
     READ     ZAMZAIL1  INVALID   KEY
              MOVE      1         TO   INV-FLG
     END-READ.
 ZAI-READ-EXIT.
     EXIT.
****************************************************************
*    LEVEL  ALL   商品在庫マスタ　初期項目編集　　　　　　　　 *
****************************************************************
 ZAI-INIT-RTN       SECTION.
     MOVE  SPACE              TO   ZAI-REC.
     INITIALIZE                    ZAI-REC.
*
     IF    ZAI-FLG     =    1
           MOVE  DNS-F08           TO   ZAI-F01
           MOVE  DNS-F03           TO   ZAI-F021
           MOVE  DNS-F04           TO   ZAI-F022
           MOVE  DNS-F10           TO   ZAI-F03
     ELSE
           MOVE  DNS-F05           TO   ZAI-F01
           MOVE  DNS-F03           TO   ZAI-F021
           MOVE  DNS-F04           TO   ZAI-F022
           MOVE  DNS-F07           TO   ZAI-F03
     END-IF.
*---<  商品名称（カナ）取得  >---*
     MOVE     ZAI-F021        TO      SUB-F011.
     MOVE     ZAI-F022        TO      SUB-F012.
     PERFORM  SUB-READ-SEC.
     MOVE     SUB-F031        TO      ZAI-F30.
*
 ZAI-INIT-EXIT.
     EXIT.
****************************************************************
*    LEVEL  ALL   商品在庫・数量セット（変更前情報）
****************************************************************
 ZAI-SUBTRACT-RTN   SECTION.
*
     IF    ZAI-FLG     =    2
*出庫情報
*T↓
*         DISPLAY "ZAI-SUBTRACT-RTN"   UPON CONS
*         DISPLAY "BEFOR    "          UPON CONS
*         DISPLAY "ZAI-F04 =" ZAI-F04  UPON CONS
*         DISPLAY "DNS-F15 =" DNS-F15  UPON CONS
*         DISPLAY "ZAI-SIME=" ZAI-SIME UPON CONS
*         DISPLAY "ZAI-F12 =" ZAI-F12  UPON CONS
*         DISPLAY "ZAI-F06 =" ZAI-F06  UPON CONS
*         DISPLAY "ZAI-F08 =" ZAI-F08  UPON CONS
*         DISPLAY "---------"          UPON CONS
*T↑
           COMPUTE  ZAI-F04  =  ZAI-F04  +  DNS-F15
           IF    DNS-F14  >  ZAI-SIME
                 COMPUTE  ZAI-F12  =  ZAI-F12  -  DNS-F15
           ELSE
                 COMPUTE  ZAI-F06  =  ZAI-F06  +  DNS-F15
                 COMPUTE  ZAI-F08  =  ZAI-F08  -  DNS-F15
           END-IF
*T↓
*         DISPLAY "AFTER    "          UPON CONS
*         DISPLAY "ZAI-F04 =" ZAI-F04  UPON CONS
*         DISPLAY "DNS-F15 =" DNS-F15  UPON CONS
*         DISPLAY "ZAI-SIME=" ZAI-SIME UPON CONS
*         DISPLAY "ZAI-F12 =" ZAI-F12  UPON CONS
*         DISPLAY "ZAI-F06 =" ZAI-F06  UPON CONS
*         DISPLAY "ZAI-F08 =" ZAI-F08  UPON CONS
*         DISPLAY "---------"          UPON CONS
*T↑
     ELSE
*入庫情報
           COMPUTE  ZAI-F04  =  ZAI-F04  -  DNS-F15
           IF    DNS-F14  >  ZAI-SIME
                 COMPUTE  ZAI-F11  =  ZAI-F11  -  DNS-F15
           ELSE
                 COMPUTE  ZAI-F06  =  ZAI-F06  -  DNS-F15
                 COMPUTE  ZAI-F07  =  ZAI-F07  -  DNS-F15
           END-IF
     END-IF.
 ZAI-SUBTRACT-EXIT.
     EXIT.
****************************************************************
*    LEVEL  ALL   商品在庫・数量セット（変更後情報）
****************************************************************
 ZAI-ADD-RTN        SECTION.
*
     IF    ZAI-FLG     =    2
*出庫情報
*T↓
*    DISPLAY "ZAI-ADD-RTN"        UPON CONS
*    DISPLAY "BEFOR    "          UPON CONS
*    DISPLAY "ZAI-F04 =" ZAI-F04  UPON CONS
*    DISPLAY "DNS-F15 =" DNS-F15  UPON CONS
*    DISPLAY "ZAI-SIME=" ZAI-SIME UPON CONS
*    DISPLAY "ZAI-F12 =" ZAI-F12  UPON CONS
*    DISPLAY "ZAI-F06 =" ZAI-F06  UPON CONS
*    DISPLAY "ZAI-F08 =" ZAI-F08  UPON CONS
*    DISPLAY "---------"          UPON CONS
*T↑
           COMPUTE  ZAI-F04  =  ZAI-F04  -  DNS-F15
           IF    DNS-F14  >  ZAI-SIME
                 COMPUTE  ZAI-F12  =  ZAI-F12  +  DNS-F15
           ELSE
                 COMPUTE  ZAI-F06  =  ZAI-F06  -  DNS-F15
                 COMPUTE  ZAI-F08  =  ZAI-F08  +  DNS-F15
           END-IF
*T↓
*    DISPLAY "AFTER    "          UPON CONS
*    DISPLAY "ZAI-F04 =" ZAI-F04  UPON CONS
*    DISPLAY "DNS-F15 =" DNS-F15  UPON CONS
*    DISPLAY "ZAI-SIME=" ZAI-SIME UPON CONS
*    DISPLAY "ZAI-F12 =" ZAI-F12  UPON CONS
*    DISPLAY "ZAI-F06 =" ZAI-F06  UPON CONS
*    DISPLAY "ZAI-F08 =" ZAI-F08  UPON CONS
*    DISPLAY "---------"          UPON CONS
*T↑
     ELSE
*入庫情報
           COMPUTE  ZAI-F04  =  ZAI-F04  +  DNS-F15
           IF    DNS-F14  >  ZAI-SIME
                 COMPUTE  ZAI-F11  =  ZAI-F11  +  DNS-F15
           ELSE
                 COMPUTE  ZAI-F06  =  ZAI-F06  +  DNS-F15
                 COMPUTE  ZAI-F07  =  ZAI-F07  +  DNS-F15
           END-IF
     END-IF.
 ZAI-ADD-EXIT.
     EXIT.
****************************************************************
*    LEVEL  3      伝票_　自動採番　　　　　　　　　　　　　　*
****************************************************************
 DPNO-SET-RTN           SECTION.
* 伝票_を自動採番する
     MOVE     60            TO  JYO-F01.
     MOVE    "NYS"          TO  JYO-F02.
     CLOSE    JYOKEN1.
*
     CALL    "SKYNSCK2"     USING    JYO-F01 JYO-F02  OUT-DENNO.
*T↓
*    DISPLAY "OUT-DENNO=" OUT-DENNO
*T↑
*    MOVE     OUT-DENNO     TO  DPNO.
     OPEN     INPUT  JYOKEN1.
*
 DPNO-SET-EXIT.
     EXIT.
****************************************************************
*    サブ商品名称マスタ検索
****************************************************************
 SUB-READ-SEC               SECTION.
     MOVE     "SUB-READ-SEC" TO   S-NAME.
*
     READ     SUBMEIL1
       INVALID
              MOVE "INV"     TO   SUB-FLG
       NOT  INVALID
              MOVE SPACE     TO   SUB-FLG
     END-READ.
 SUB-READ-EXIT.
     EXIT.
****************************************************************
*    終了
****************************************************************
 END-SEC                   SECTION.
*
     MOVE     "END-SEC"     TO    S-NAME.
*
     MOVE      IDO-RD-CNT   TO    MSG-OUT01.
     MOVE      DNS-WT-CNT   TO    MSG-OUT02.
*    MOVE      DNS-RT-CNT   TO    MSG-OUT03.
     MOVE      ZAI-WT-CNT   TO    MSG-OUT04.
     MOVE      ZAI-RT-CNT   TO    MSG-OUT05.
     DISPLAY   MSG-OUT1-FIL1 MSG-OUT1-FIL2 MSG-OUT01
               MSG-OUT1-FIL3 MSG-OUT1-FIL4 UPON CONS.
     DISPLAY   MSG-OUT2-FIL1 MSG-OUT2-FIL2 MSG-OUT02
               MSG-OUT2-FIL3 MSG-OUT2-FIL4 UPON CONS.
*    DISPLAY   MSG-OUT3-FIL1 MSG-OUT3-FIL2 MSG-OUT03
*              MSG-OUT3-FIL3 MSG-OUT3-FIL4 UPON CONS.
     DISPLAY   MSG-OUT4-FIL1 MSG-OUT4-FIL2 MSG-OUT04
               MSG-OUT4-FIL3 MSG-OUT4-FIL4 UPON CONS.
     DISPLAY   MSG-OUT5-FIL1 MSG-OUT5-FIL2 MSG-OUT05
               MSG-OUT5-FIL3 MSG-OUT5-FIL4 UPON CONS.
*
     CLOSE     IDOXXXW3
               DNSFILL1
               SUBMEIL1
               JYOKEN1
               ZAMZAIL1.
*
     DISPLAY   MSG-END      UPON  CONS.
*
 END-EXIT.
     EXIT.
