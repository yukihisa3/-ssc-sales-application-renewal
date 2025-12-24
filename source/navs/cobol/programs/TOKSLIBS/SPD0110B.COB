****************************************************************
*                                                              *
*    顧客名　　　　　　　：　サカタのタネ（株）殿　　　　　　　*
*    業務名　　　　　　　：　在庫管理システム改善　　　　　　　*
*    モジュール名　　　　：　取込確認リスト兼エラーリスト      *
*                  　　　：　SPD0110B                          *
*    作成日／作成者　　　：　2009/10/14   OONO                 *
*    更新日／更新者　　　：　                                  *
*    処理概要　　　　　　：　量販店ＪＮＬ（ＰＣ連携用）を読み、*
*                            各項目のチェックを行う。エラーの場*
*                            合はエラー区分代表に”１”をセット*
*                            すると共に、各エラー項目の指定エラ*
*                            区分位置に”Ｅ”を更新する        *
*    更新日／更新者　　　：　2010/02/18   OONO                 *
*    処理概要　　　　　　：　量販店ＪＮＬ（ＰＣ連携用）の原単、*
*                            売単を商品変換ＴＢＬで更新する様に*
*                            変更。                            *
****************************************************************
 IDENTIFICATION              DIVISION.
 PROGRAM-ID.                 SPD0110B.
 ENVIRONMENT                 DIVISION.
 CONFIGURATION               SECTION.
 SOURCE-COMPUTER.
 OBJECT-COMPUTER.
 SPECIAL-NAMES.
     YA            IS        CHR-2
     YB-21         IS        CHR-21
     YB            IS        CHR-15
     CONSOLE       IS        CONS
     STATION       IS        STA.
****************************************************************
 INPUT-OUTPUT              SECTION.
****************************************************************
 FILE-CONTROL.
*量販店ＪＮＬ（ＰＣ連携用）
     SELECT      PCRYOJF     ASSIGN    TO        PCRYOJL1
                             ORGANIZATION        INDEXED
                             ACCESS    MODE      SEQUENTIAL
                             RECORD    KEY       PCR-F011
                                                 PCR-F012
                                                 PCR-F02
                             FILE      STATUS    PCR-ST.
*取引先マスタ
     SELECT      HTOKMS      ASSIGN    TO        DA-01-VI-TOKMS2
                             ORGANIZATION        INDEXED
                             ACCESS    MODE      RANDOM
                             RECORD    KEY       TOK-F01
                             FILE      STATUS    TOK-ST.
*店舗マスタ
     SELECT      HTENMS      ASSIGN    TO        DA-01-VI-TENMS1
                             ORGANIZATION        INDEXED
                             ACCESS    MODE      RANDOM
                             RECORD    KEY       TEN-F52
                                                 TEN-F011
                             FILE      STATUS    TEN-ST.
*商品コード変換テーブル
     SELECT     HSHOTBL      ASSIGN    TO        DA-01-VI-SHOTBL1
                             ORGANIZATION        INDEXED
                             ACCESS    MODE      RANDOM
                             RECORD    KEY       SHO-F01
                                                 SHO-F02
                             FILE      STATUS    SHO-ST.
*商品名称マスタ
     SELECT     HMEIMS       ASSIGN    TO        DA-01-VI-MEIMS1
                             ORGANIZATION        INDEXED
                             ACCESS    MODE      RANDOM
                             RECORD    KEY       MEI-F01
                             FILE      STATUS    MEI-ST.
*条件ファイル
     SELECT     HJYOKEN      ASSIGN    TO        DA-01-VI-JYOKEN1
                             ORGANIZATION        INDEXED
                             ACCESS    MODE      RANDOM
                             RECORD    KEY       JYO-F01
                                                 JYO-F02
                             FILE      STATUS    JYO-ST.
****************************************************************
 DATA                        DIVISION.
****************************************************************
 FILE                        SECTION.
*量販店ＪＮＬ（ＰＣ連携用）
 FD  PCRYOJF
     LABEL       RECORD      IS        STANDARD.
     COPY        PCRYOJL1    OF        XFDLIB
     JOINING     PCR         AS        PREFIX.
*取引先マスタ
 FD  HTOKMS
     LABEL       RECORD      IS        STANDARD
     BLOCK       CONTAINS     8        RECORDS.
     COPY        HTOKMS      OF        XFDLIB
     JOINING     TOK         AS        PREFIX.
*店舗マスタ
 FD  HTENMS.
     COPY     HTENMS    OF        XFDLIB
              JOINING   TEN       PREFIX.
*商品変換テーブル
 FD  HSHOTBL.
     COPY     HSHOTBL   OF        XFDLIB
              JOINING   SHO       PREFIX.
*商品名称マスタ
 FD  HMEIMS.
     COPY     HMEIMS    OF        XFDLIB
              JOINING   MEI       PREFIX.
*条件ファイル
 FD  HJYOKEN.
     COPY     HJYOKEN   OF        XFDLIB
              JOINING   JYO       PREFIX.
****************************************************************
 WORKING-STORAGE           SECTION.
****************************************************************
 01  ST-AREA.
     03  IN-DATA             PIC  X(01)  VALUE  SPACE.
     03  PCR-ST              PIC  X(02)  VALUE  SPACE.
     03  TOK-ST              PIC  X(02)  VALUE  SPACE.
     03  TEN-ST              PIC  X(02)  VALUE  SPACE.
     03  SHO-ST              PIC  X(02)  VALUE  SPACE.
     03  MEI-ST              PIC  X(02)  VALUE  SPACE.
     03  JYO-ST              PIC  X(02)  VALUE  SPACE.
 01  WK-AREA.
     03  IN-CNT              PIC  9(06)  VALUE  0.
     03  OK-CNT              PIC  9(06)  VALUE  0.
     03  ERR-CNT             PIC  9(06)  VALUE  0.
     03  END-FLG             PIC  9(01)  VALUE  ZERO.
     03  ERR-FLG             PIC  X(01)  VALUE  SPACE.
     03  INV-FLG             PIC  X(03)  VALUE  SPACE.
     03  GETUDO              PIC  9(02).
     03  WK-GETUDO           PIC  9(02).
     03  L-CNT               PIC  99     VALUE  ZERO.
     03  P-CNT               PIC  99     VALUE  ZERO.
     03  TOK-INV-FLG         PIC  X(03)  VALUE  SPACE.
     03  HSHOTBL-INV-FLG     PIC  X(03)  VALUE  SPACE.
     03  HMEIMS-INV-FLG      PIC  X(03)  VALUE  SPACE.
     03  HTENMS-INV-FLG      PIC  X(03)  VALUE  SPACE.
     03  HZK-ERR-FLG         PIC  X(03)  VALUE  SPACE.
     03  ACOS-ERR-FLG        PIC  X(03)  VALUE  SPACE.
*日付取得
 01  SYS-DATE                PIC  9(06)  VALUE  ZERO.
 01  SYS-DATER    REDEFINES  SYS-DATE.
     03  SYS-YY              PIC  9(02).
     03  SYS-MM              PIC  9(02).
     03  SYS-DD              PIC  9(02).
 01  WK-STR-YM               PIC  9(06).
 01  WK-STR-YM-R  REDEFINES  WK-STR-YM.
     03  WK-STR-YY           PIC  9(04).
     03  WK-STR-MM           PIC  9(02).
 01  WK-END-YM               PIC  9(06).
 01  WK-END-YM-R  REDEFINES  WK-END-YM.
     03  WK-END-YY           PIC  9(04).
     03  WK-END-MM           PIC  9(02).
 01  WK-SIME                 PIC  9(06).
 01  ACOS-DATE               PIC  9(08).
 01  WK-ACOS-YMD       REDEFINES  ACOS-DATE.
     03  WK-ACOS-YM          PIC  9(06).
     03  WK-ACOS-DD          PIC  9(02).
 01  WK-GETUDO2-YMD          PIC  9(08).
 01  WK-GETUDO2-YMD-R  REDEFINES  WK-GETUDO2-YMD.
     03  WK-GETUDO2-YY       PIC  9(04).
     03  WK-GETUDO2-MM       PIC  9(02).
     03  WK-GETUDO2-DD       PIC  9(02).
 01  WK-NOHIN-YMD            PIC  9(08).
 01  WK-NOHIN2-YMD     REDEFINES  WK-NOHIN-YMD.
     03  WK-NOHIN-YM         PIC  9(06).
     03  WK-NOHIN-DD         PIC  9(02).
**
 01  WK-WYYMM                PIC  9(06).
 01  WK-WYYMM-R        REDEFINES  WK-WYYMM.
     03  WK-WYY              PIC  9(04).
     03  WK-WMM              PIC  9(02).
 01  WK-DATE8.
     03  WK-DATE-Y           PIC  9(04)  VALUE  ZERO.
     03  WK-DATE-M           PIC  9(02)  VALUE  ZERO.
     03  WK-DATE-D           PIC  9(02)  VALUE  ZERO.
 01  SYS-TIME           PIC  9(08).
 01  SYS-TIMER          REDEFINES      SYS-TIME.
     03  SYS-HH         PIC  9(02).
     03  SYS-MN         PIC  9(02).
     03  SYS-SS         PIC  9(02).
     03  SYS-MS         PIC  9(02).
 01  SYS-TIME2          PIC  9(08).
 01  SYS-TIME2R         REDEFINES      SYS-TIME2.
     03  SYS-TIMEW      PIC  9(06).
     03  FILLER         PIC  9(02).
*
 01  WK-TIME-DSP.
     03  WK-TIME-HH     PIC  9(02).
     03  WK-TIME-MN     PIC  9(02).
***  エラーセクション名
 01  SEC-NAME.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-SEC    => ".
     03  S-NAME                   PIC  X(20).
*
*
 01  FILE-ERR.
     03  PCR-ERR             PIC  N(15)  VALUE
                   NC"量販店ＪＮＬ（ＰＣ連携用）異常".
     03  TEN-ERR             PIC  N(10)  VALUE
                   NC"店舗マスタ異常".
     03  SHO-ERR             PIC  N(10)  VALUE
                   NC"商品変換テーブル異常".
     03  MEI-ERR             PIC  N(10)  VALUE
                   NC"商品名称マスタ異常".
     03  JYO-ERR             PIC  N(10)  VALUE
                   NC"条件ファイル異常".
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN             PIC X(01).
 01  LINK-IN-YMD6            PIC 9(06).
 01  LINK-IN-YMD8            PIC 9(08).
 01  LINK-OUT-RET            PIC X(01).
 01  LINK-OUT-YMD            PIC 9(08).
****************************************************************
 LINKAGE                     SECTION.
 01  PARA-BUMON             PIC   X(04).
 01  PARA-TANCD             PIC   X(02).
 01  PARA-MEMOST            PIC   9(04).
 01  PARA-MEMOEN            PIC   9(04).
****************************************************************
****************************************************************
 PROCEDURE             DIVISION  USING  PARA-BUMON  PARA-TANCD
                                 PARA-MEMOST  PARA-MEMOEN.
****************************************************************
 DECLARATIVES.
 PCR-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE PCRYOJF.
     DISPLAY     PCR-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     PCR-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
***  ACCEPT      IN-DATA     FROM      CONS.
     STOP        RUN.
 TEN-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE HTENMS.
     DISPLAY     TEN-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     TEN-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
***  ACCEPT      IN-DATA     FROM      CONS.
     STOP        RUN.
 SHO-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE HSHOTBL.
     DISPLAY     SHO-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     SHO-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
***  ACCEPT      IN-DATA     FROM      CONS.
     STOP        RUN.
 MEI-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE HMEIMS.
     DISPLAY     MEI-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     MEI-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
***  ACCEPT      IN-DATA     FROM      CONS.
     STOP        RUN.
 JYO-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE HJYOKEN.
     DISPLAY     JYO-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     JYO-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
***  ACCEPT      IN-DATA     FROM      CONS.
     STOP        RUN.
 END DECLARATIVES.
****************************************************************
*                 P R O G R A M - S E C
****************************************************************
 PROGRAM-SEC                 SECTION.
     PERFORM     INIT-SEC.
     PERFORM     MAIN-SEC    UNTIL     END-FLG  = 9.
     PERFORM     END-SEC.
     STOP        RUN.
*PROGRAM-END.
****************************************************************
*                 I N I T - S E C
****************************************************************
 INIT-SEC                    SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
*
     DISPLAY  "＊＊取込みデータチェック開始＊＊" UPON STA.
     DISPLAY  "メモ_開始：" PARA-MEMOST " 終了：" PARA-MEMOEN
              UPON CONS.
     OPEN        INPUT       HTOKMS    HTENMS
                             HSHOTBL   HMEIMS  HJYOKEN
                 I-O         PCRYOJF.
 INIT010.
*システム日付・時刻の取得
     ACCEPT   SYS-DATE          FROM   DATE.
     ACCEPT   SYS-TIME          FROM   TIME.
**
 INIT020.
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
 INIT030.
     MOVE      LINK-OUT-YMD       TO   WK-DATE8.
 INIT031.
*    MOVE     WK-DATE8           TO   PARA-O-DATE.
*    DISPLAY NC"＃＃基準日付" " = "   PARA-O-DATE    UPON CONS.
*    MOVE     SYS-HH             TO   WK-TIME-HH.
*    MOVE     SYS-MN             TO   WK-TIME-MN.
*    MOVE     WK-TIME-DSP        TO   PARA-O-TIME.
*    DISPLAY NC"＃＃基準時刻" " = "   PARA-O-TIME    UPON CONS.
*条件ファイル読込処理
*条件Ｆ（経理月）
     MOVE     "58"           TO   JYO-F01.
     MOVE     SPACE          TO   JYO-F02.
     PERFORM  JYO-RD-RTN.
     IF       INV-FLG  =  "INV"
              DISPLAY   "HJYOKEN INV KEY=58"  UPON CONS
              MOVE        4000   TO        PROGRAM-STATUS
              STOP        RUN
     END-IF.
     MOVE     JYO-F04        TO   GETUDO.
     MOVE     JYO-F04        TO   WK-GETUDO.
*条件Ｆ（ＡＣＯＳ用締日）
     MOVE     "99"           TO   JYO-F01.
     MOVE     SPACE          TO   JYO-F02.
     PERFORM  JYO-RD-RTN.
     IF       INV-FLG  =  "INV"
              DISPLAY   "HJYOKEN INV KEY=99"  UPON CONS
              MOVE        4000   TO        PROGRAM-STATUS
              STOP        RUN
     END-IF.
     EVALUATE GETUDO
         WHEN 1
              MOVE      JYO-F04        TO   ACOS-DATE
         WHEN 2
              MOVE      JYO-F05        TO   ACOS-DATE
         WHEN 3
              MOVE      JYO-F06        TO   ACOS-DATE
         WHEN 4
              MOVE      JYO-F07        TO   ACOS-DATE
         WHEN 5
              MOVE      JYO-F08        TO   ACOS-DATE
         WHEN 6
              MOVE      JYO-F09        TO   ACOS-DATE
         WHEN 7
              MOVE      JYO-F10        TO   ACOS-DATE
         WHEN 8
              MOVE      JYO-F11        TO   ACOS-DATE
         WHEN 9
              MOVE      JYO-F12        TO   ACOS-DATE
         WHEN 10
              MOVE      JYO-F12A       TO   ACOS-DATE
         WHEN 11
              MOVE      JYO-F12B       TO   ACOS-DATE
         WHEN 12
              MOVE      JYO-F12C       TO   ACOS-DATE
         WHEN OTHER
              DISPLAY  "### ｹﾞﾂﾄﾞ ｲｼﾞｮｳ  ｹﾞﾂﾄﾞ= "
                         GETUDO  " ###"   UPON CONS
              STOP   RUN
**
     END-EVALUATE.
*条件Ｆ（在庫締日）
     MOVE     "99"           TO   JYO-F01.
     MOVE     "ZAI"          TO   JYO-F02.
     PERFORM  JYO-RD-RTN.
     IF       INV-FLG  =  "INV"
              DISPLAY   "HJYOKEN INV KEY=99"  UPON CONS
              MOVE        4000   TO        PROGRAM-STATUS
              STOP        RUN
     END-IF.
     MOVE     JYO-F05        TO   WK-SIME.
     MOVE     WK-SIME        TO   WK-WYYMM.
     ADD      1              TO   WK-WMM.
     IF       WK-WMM     >    12
              ADD       1    TO   WK-WYY
              MOVE      1    TO   WK-WMM
     END-IF.
     MOVE     WK-WYYMM        TO   WK-SIME.
* 開始年月
     MOVE     ACOS-DATE      TO   WK-GETUDO2-YMD.
     MOVE     99             TO   WK-GETUDO2-DD.
     IF    WK-GETUDO         =    12
              COMPUTE     WK-GETUDO2-YY     =
                          WK-GETUDO2-YY     -        1
              MOVE     12    TO          WK-GETUDO2-MM
     ELSE
              MOVE   WK-GETUDO    TO     WK-GETUDO2-MM
     END-IF.
* 終了年月
     MOVE     WK-DATE-Y      TO   WK-STR-YY
                                  WK-END-YY.
     MOVE     WK-DATE-M      TO   WK-STR-MM
                                  WK-END-MM.
     ADD      1              TO   WK-END-MM.
     IF    WK-END-MM  >  12
              MOVE      1         TO   WK-END-MM
              COMPUTE   WK-END-YY   =  WK-DATE-Y  +  1
     ELSE
              MOVE      WK-DATE-Y   TO   WK-END-YY
     END-IF.
* 不要ステップ
**## SUBTRACT 1              FROM WK-STR-MM.
**## IF       WK-STR-MM  <  1
**##          MOVE      12        TO   WK-STR-MM
**##          COMPUTE   WK-STR-YY  =  SYS-YY  -  1
**## ELSE
**##          MOVE         SYS-YY      TO   WK-STR-YY
**## END-IF.
*
     MOVE  WK-GETUDO2-YY     TO   WK-STR-YY.
     MOVE  WK-GETUDO2-MM     TO   WK-STR-MM.
**
     DISPLAY NC"％％　条件ファイル検索　％％" UPON STA.
     DISPLAY NC"％％ＡＣＯＳ締日＝"  ACOS-DATE  UPON STA.
     DISPLAY NC"％％開始年月＝"   WK-STR-YM   UPON STA.
     DISPLAY NC"％％終了年月＝"   WK-END-YM   UPON STA.
*
*量販店ＪＮＬ（ＰＣ連携用）にスタートを掛ける
     MOVE     PARA-MEMOST    TO        PCR-F011.
*    MOVE     PARA-MEMOEN    TO        PCR-F012.
     MOVE     ZERO           TO        PCR-F012  PCR-F02.
*
     START    PCRYOJF   KEY  >=        PCR-F011   PCR-F012
                                       PCR-F02
              INVALID
                   MOVE      9         TO   END-FLG
                   DISPLAY   NC"指定メモＮｏ開始が無い" UPON STA
                   GO                  TO   INIT-EXIT
     END-START.
*
*ＰＣ手書データ連携ファイル初期読み込み
     PERFORM  PCRYOJ-RD-SEC.
*メモ１が指定パラメタ以上の場合は処理終了
     IF       PCR-F011  >    PARA-MEMOEN
              MOVE      9         TO   END-FLG
              DISPLAY
              NC"メモ番号１が指定メモＮｏ終了を超えた" UPON STA
     END-IF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*                 M A I N - S E C
****************************************************************
 MAIN-SEC                    SECTION.
     MOVE     "MAIN-SEC"          TO   S-NAME.
*
     MOVE   SPACE       TO  ERR-FLG.
*取引先コードチェック*(1)
     PERFORM  TOK-RD-SEC.
*店舗コードチェック*(2)
     PERFORM  HTENMS-READ-SEC.
*納品日チェック*(3)
     PERFORM  NOUHIN-HZKCHK-SEC.
*ＡＣＯＳ締日チェック*(4)
     PERFORM  ACOS-HZKCHK-SEC.
*商品変換テーブルチェック*(5)
     PERFORM  HSHOTBL-READ-SEC.
*商品名称マスタチェック*(6)
     PERFORM  HMEIMS-READ-SEC.
* エラー判定
     IF  ERR-FLG  =  "Y"
         PERFORM  PCR-WT-SEC
     ELSE
         PERFORM  PCR-OK-SEC
     END-IF.
*ＰＣ手書データ連携ファイル読込み
     PERFORM  PCRYOJ-RD-SEC.
 MAIN-EXIT.
     EXIT.
****************************************************************
*               量販店ＪＮＬ（ＰＣ連携用）ＲＥＡＤ
****************************************************************
 PCRYOJ-RD-SEC                 SECTION.
     MOVE     "PCRYOJ-RD-SEC"       TO   S-NAME.
*
     READ     PCRYOJF   AT   END
              MOVE      9         TO   END-FLG
              GO        TO        PCRYOJ-RD-EXIT
     END-READ.
*メモ１が指定パラメタ以上の場合は処理終了
*****DISPLAY "PARA-MEMOEN = " PARA-MEMOEN UPON CONS.
*****DISPLAY "PCR-F011    = " PCR-F011    UPON CONS.
     IF       PCR-F011  >    PARA-MEMOEN
              MOVE      9         TO   END-FLG
              GO        TO        PCRYOJ-RD-EXIT
     END-IF.
*
     ADD   1   TO  IN-CNT.
 PCRYOJ-RD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*                 条件ファイル　　　ＲＥＡＤ　　　　　　　　　 *
*--------------------------------------------------------------*
 JYO-RD-RTN           SECTION.
     MOVE     "JYO-RD-RTN"       TO   S-NAME.
     MOVE     SPACE     TO   INV-FLG.
     READ     HJYOKEN   INVALID   KEY
              MOVE      "INV"     TO   INV-FLG
              GO   TO   JYO-RD-EXIT
     END-READ.
 JYO-RD-EXIT.
     EXIT.
****************************************************************
*              手書データエラーＦ ＲＥＷＲＩＴＥ
****************************************************************
 PCR-WT-SEC                 SECTION.
     MOVE     "PCR-WT-SEC"       TO   S-NAME.
*
*エラー区分に空白をセット
     MOVE      SPACE     TO        PCR-F574   PCR-F575
                                   PCR-F576   PCR-F577
                                   PCR-F578   PCR-F579
                                   PCR-F57A.
*エラー区分セット
     MOVE     "E"        TO        PCR-F574.
*
     IF  TOK-INV-FLG  = "INV"
         MOVE    "E"         TO        PCR-F575.
     IF  HTENMS-INV-FLG  = "INV"
         MOVE    "E"         TO        PCR-F576.
     IF  HZK-ERR-FLG  = "ERR"
         MOVE    "E"         TO        PCR-F577.
     IF  ACOS-ERR-FLG  = "ERR"
         MOVE    "E"         TO        PCR-F578.
     IF  HSHOTBL-INV-FLG  = "INV"
         MOVE    "E"         TO        PCR-F579
     ELSE
         IF  PCR-F131  =  ZERO
             MOVE SHO-F05    TO        PCR-F131
         END-IF
     END-IF.
     IF  HMEIMS-INV-FLG  = "INV"
         MOVE    "E"         TO        PCR-F57A.
*
     REWRITE  PCR-REC.
     ADD      1  TO  ERR-CNT.
*
 PCR-WT-EXIT.
     EXIT.
****************************************************************
*              手書データエラーＦ ＲＥＷＲＩＴＥ
****************************************************************
 PCR-OK-SEC                 SECTION.
     MOVE     "PCR-OK-SEC"       TO   S-NAME.
*
*エラー区分に空白をセット
     MOVE      SPACE     TO        PCR-F574   PCR-F575
                                   PCR-F576   PCR-F577
                                   PCR-F578   PCR-F579
                                   PCR-F57A.
*原価単価取得
*****DISPLAY "HSHOTBL-INV-FLG = " HSHOTBL-INV-FLG UPON CONS.
     IF  HSHOTBL-INV-FLG  =  SPACE
         IF  PCR-F131  =  ZERO
             MOVE SHO-F05    TO        PCR-F131
*************DISPLAY "AAAAA" UPON CONS
         END-IF
     END-IF.
*
     REWRITE  PCR-REC.
*
 PCR-OK-EXIT.
     EXIT.
****************************************************************
*             納品日チェック処理                               *
****************************************************************
 NOUHIN-HZKCHK-SEC           SECTION.
     MOVE     "NOUHIN-HZKCHK-SEC"       TO   S-NAME.
*
***  開始年月 =< 納品日年月 =< 終了年月　の範囲
     MOVE    PCR-F072       TO   WK-NOHIN-YMD.
     IF ( WK-STR-YM     >   WK-NOHIN-YM )  OR
        ( WK-NOHIN-YM   >   WK-END-YM )
         MOVE    "ERR"    TO  HZK-ERR-FLG
         MOVE      "Y"    TO   ERR-FLG
     ELSE
         MOVE   SPACE     TO  HZK-ERR-FLG
     END-IF.
*
 NOUHIN-HZKCHK-EXIT.
     EXIT.
****************************************************************
*             ＡＣＯＳ締日チェック処理                         *
****************************************************************
 ACOS-HZKCHK-SEC           SECTION.
     MOVE     "ACOS-HZKCHK-SEC"       TO   S-NAME.
*
***  ＡＣＯＳ締日 < システム日付  かつ
***  ＡＣＯＳ締日年月 < 納品日年月　はエラー
     MOVE    PCR-F072       TO   WK-NOHIN-YMD.
     IF  ( ACOS-DATE   <  WK-DATE8 ) AND
         ( WK-ACOS-YM  <  WK-NOHIN-YM )
         MOVE    "ERR"    TO  ACOS-ERR-FLG
         MOVE      "Y"    TO   ERR-FLG
     ELSE
         MOVE    SPACE    TO  ACOS-ERR-FLG
     END-IF.
*
 ACOS-HZKCHK-EXIT.
     EXIT.
****************************************************************
*               取 引 先 マ ス タ Ｒ Ｅ Ａ Ｄ
****************************************************************
 TOK-RD-SEC                  SECTION.
     MOVE     "TOK-RD-SEC"        TO   S-NAME.
*
     MOVE     PCR-F11             TO   TOK-F01.
     READ     HTOKMS
       INVALID
              MOVE      "INV"     TO   TOK-INV-FLG
              MOVE      "Y"       TO   ERR-FLG
       NOT INVALID
              MOVE      SPACE     TO   TOK-INV-FLG
*
     END-READ.
 TOK-RD-EXIT.
     EXIT.
****************************************************************
*                店舗マスタ読込み                              *
****************************************************************
 HTENMS-READ-SEC           SECTION.
*
     MOVE      "HTENMS-READ-SEC"  TO    S-NAME.
*
     MOVE      PCR-F11     TO     TEN-F52.
     MOVE      PCR-F161(05)    TO     TEN-F011.
     READ      HTENMS
         INVALID
               MOVE      "INV"    TO    HTENMS-INV-FLG
               MOVE      "Y"       TO   ERR-FLG
         NOT  INVALID
               MOVE      SPACE    TO    HTENMS-INV-FLG
     END-READ.
*
 HTENMS-READ-EXIT.
     EXIT.
****************************************************************
*                商品変換テーブル読込み                        *
****************************************************************
 HSHOTBL-READ-SEC          SECTION.
*
     MOVE      "HSHOTBL-READ-SEC" TO    S-NAME.
*
* ADD NAV OONO 2010/02/18 BGN
     MOVE      SPACE       TO     SHO-REC.
     INITIALIZE                   SHO-REC.
* ADD NAV OONO 2010/02/18 END
     MOVE      PCR-F11     TO     SHO-F01.
     MOVE      PCR-F121    TO     SHO-F02.
     READ      HSHOTBL
         INVALID
               MOVE      "INV"    TO    HSHOTBL-INV-FLG
               MOVE      "Y"      TO   ERR-FLG
               MOVE      ZERO     TO    SHO-F09
*              ADD NAV OONO 2010/02/18 BGN
               MOVE      ZERO     TO    PCR-F131 PCR-F132
*              ADD NAV OONO 2010/02/18 END
         NOT  INVALID
               MOVE      SPACE    TO    HSHOTBL-INV-FLG
*              ADD NAV OONO 2010/02/18 BGN
               MOVE      SHO-F05  TO    PCR-F131
               MOVE      SHO-F06  TO    PCR-F132
*              ADD NAV OONO 2010/02/18 END
     END-READ.
*
 HSHOTBL-READ-EXIT.
     EXIT.
****************************************************************
*                商品名称マスタ読込み                          *
****************************************************************
 HMEIMS-READ-SEC           SECTION.
*
     MOVE      "HMEIMS-READ-SEC"  TO    S-NAME.
     IF  HSHOTBL-INV-FLG  = "INV"
         MOVE    SPACE       TO        HMEIMS-INV-FLG
         GO  TO        HMEIMS-READ-EXIT
     END-IF.
*
     MOVE      SHO-F031    TO     MEI-F011.
     MOVE      SHO-F032    TO     MEI-F012.
     READ      HMEIMS
         INVALID
               MOVE      "INV"    TO    HMEIMS-INV-FLG
               MOVE      "Y"       TO   ERR-FLG
         NOT  INVALID
               MOVE      SPACE    TO    HMEIMS-INV-FLG
     END-READ.
*
 HMEIMS-READ-EXIT.
     EXIT.
****************************************************************
*    終了
****************************************************************
 END-SEC                   SECTION.
*
      DISPLAY  "＊＊取込みデータチェック終了＊＊" UPON STA.
*
     CLOSE PCRYOJF  HTOKMS  HTENMS
           HJYOKEN  HSHOTBL HMEIMS.
 END-EXIT.
     EXIT.
