# SFU3030B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SFU3030B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　社内振替　　　　　　　　　　　　　*
*    モジュール名　　　　：　社内振替発注ＥＸＣＥＬ取込        *
*    作成日／作成者　　　：　2016/12/27 INOUE                  *
*    処理内容　　　　　　：　取込チェックにてＯＫとなった　　　*
*    　　　　　　　　　　　　データより、社内振替情報ファイル、*
*                            明細ファイル作成を行う。　　　　　*
*    変更日／作成者　　　：　2017/05/01 TAKAHASHI              *
*    変更内容　　　　　　：　在庫更新機能を追加（未入庫数）    *
*    変更日／作成者　　　：　2018/05/01 TAKAHASHI              *
*    変更内容　　　　　　：　明細情報部分を上書き更新可能に変更*
****************************************************************
 IDENTIFICATION              DIVISION.
 PROGRAM-ID.                 SFU3030B.
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
*振替ＥＸＣＥＬ取込ファイル
     SELECT      SFRWKL1     ASSIGN    TO       DA-01-VI-SFRWKL1
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     SEQUENTIAL
                             RECORD    KEY      EXL-F051
                                                EXL-F052
                                                EXL-F053
                                                EXL-F054
                                                EXL-F055
                                                EXL-F056
                                                EXL-F057
                                                EXL-F058
                             FILE      STATUS   EXL-ST.
*振替情報ファイル
     SELECT      SFRHEDL1    ASSIGN    TO       DA-01-VI-SFRHEDL1
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     DYNAMIC
                             RECORD    KEY      HED-F01
                                                HED-F02
                                                HED-F03
                                                HED-F04
                                                HED-F05
                                                HED-F06
                                                HED-F07
                                                HED-F08
                             FILE      STATUS   HED-ST.
*振替明細ファイル
     SELECT      SFRMEIL1    ASSIGN    TO       DA-01-VI-SFRMEIL1
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     DYNAMIC
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
*商品在庫マスタ
     SELECT      ZAMZAIL1  ASSIGN    TO        DA-01-VI-ZAMZAIL1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       ZAI-F01
                                               ZAI-F021
                                               ZAI-F022
                                               ZAI-F03
                           FILE      STATUS    ZAI-ST.
*商品名称マスタ
     SELECT      MEIMS1    ASSIGN    TO        DA-01-VI-MEIMS1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       MES-F011
                                               MES-F0121
                                               MES-F0122
                                               MES-F0123
                           FILE      STATUS    MES-ST.
****************************************************************
 DATA                        DIVISION.
****************************************************************
 FILE                        SECTION.
*振替ＥＸＣＥＬ取込Ｆ
 FD  SFRWKL1.
     COPY        SFRWKL1     OF        XFDLIB
     JOINING     EXL         AS        PREFIX.
*振替情報ファイル
 FD  SFRHEDL1.
     COPY        SFRHEDL1    OF        XFDLIB
     JOINING     HED         AS        PREFIX.
*振替明細ファイル
 FD  SFRMEIL1.
     COPY        SFRMEIL1    OF        XFDLIB
     JOINING     MEI         AS        PREFIX.
*商品在庫マスタ
 FD  ZAMZAIL1.
     COPY        ZAMZAIF     OF        XFDLIB
     JOINING     ZAI         AS        PREFIX.
*商品名称マスタ
 FD  MEIMS1.
     COPY        HMEIMS      OF        XFDLIB
     JOINING     MES         AS        PREFIX.
****************************************************************
 WORKING-STORAGE           SECTION.
****************************************************************
 01  ST-AREA.
     03  EXL-ST              PIC  X(02)  VALUE  SPACE.
     03  MEI-ST              PIC  X(02)  VALUE  SPACE.
     03  HED-ST              PIC  X(02)  VALUE  SPACE.
     03  ZAI-ST              PIC  X(02)  VALUE  SPACE.
     03  MES-ST              PIC  X(02)  VALUE  SPACE.
 01  SV-AREA.
     03  BR-EXL-F051         PIC  X(02)  VALUE  SPACE.
     03  BR-EXL-F052         PIC  9(04)  VALUE  ZERO.
     03  BR-EXL-F053         PIC  X(02)  VALUE  SPACE.
     03  BR-EXL-F054         PIC  X(02)  VALUE  SPACE.
     03  BR-EXL-F055         PIC  X(08)  VALUE  SPACE.
     03  BR-EXL-F056         PIC  X(05)  VALUE  SPACE.
     03  BR-EXL-F057         PIC  X(02)  VALUE  SPACE.
     03  BR-EXL-F058         PIC  X(01)  VALUE  SPACE.
 01  WK-AREA.
     03  DPNO                PIC  9(07)  VALUE  ZERO.
     03  I                   PIC  9(02)  VALUE  ZERO.
     03  END-FLG             PIC  X(03)  VALUE  SPACE.
     03  EXL-ENDFLG          PIC  X(01)  VALUE  SPACE.
     03  EXL-RD-CNT          PIC  9(07)  VALUE  ZERO.
     03  HED-WT-CNT          PIC  9(07)  VALUE  ZERO.
     03  MEI-WT-CNT          PIC  9(07)  VALUE  ZERO.
     03  HED-RW-CNT          PIC  9(07)  VALUE  ZERO.
     03  MEI-RW-CNT          PIC  9(07)  VALUE  ZERO.
**
 01  HED-INV-FLG             PIC  X(03)  VALUE  SPACE.
 01  MEI-INV-FLG             PIC  X(03)  VALUE  SPACE.
 01  HMEIMS-INV-FLG          PIC  X(03)  VALUE  SPACE.
*#2017/05/01 NAV ST -------------------------------------*
 01  WK-BAK-TANBAN           PIC  X(06)  VALUE  SPACE.
 01  WK-BAK-HNSU             PIC S9(07)  VALUE  ZERO.
 01  WK-KEY-SOKCD            PIC  X(02)  VALUE  SPACE.
 01  WK-KEY-SYOCD            PIC  X(08)  VALUE  SPACE.
 01  WK-KEY-HINCD            PIC  X(08)  VALUE  SPACE.
 01  WK-KEY-TANBAN           PIC  X(06)  VALUE  SPACE.
 01  WK-SURYO                PIC S9(07)  VALUE  ZERO.
 01  WK-SURYOZAN             PIC S9(07)  VALUE  ZERO.
*#2017/05/01 NAV ED -------------------------------------*
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
 01  WK-SAGYOUBI-1           PIC  9(08)  VALUE  ZERO.
 01  WK-SAGYOUBI-1-YMD       REDEFINES   WK-SAGYOUBI-1.
     03  WK-SAGYOUBI-Y       PIC  9(04).
     03  WK-SAGYOUBI-M       PIC  9(02).
     03  WK-SAGYOUBI-D       PIC  9(02).
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
*経理〆日
 01  WK-SIME                 PIC  9(08).
*
*在庫〆日
 01  ZAI-SIME.
     03  ZAI-SIME1           PIC  9(06)     VALUE ZERO.
     03  ZAI-SIME1R          REDEFINES      ZAI-SIME1.
         05  ZAI-SIME1R1     PIC  9(04).
         05  ZAI-SIME1R2     PIC  9(02).
     03  ZAI-SIME2           PIC  9(02)     VALUE ZERO.
*
 01  WK-ZAIKO-SIME           PIC  9(09).
 01  FILLER                  REDEFINES   WK-ZAIKO-SIME.
     03  WK-ZAIKO-SIME-0     PIC  9(01).
     03  WK-ZAIKO-SIME-1     PIC  9(04).
     03  WK-ZAIKO-SIME-2     PIC  9(02).
     03  WK-ZAIKO-SIME-3     PIC  9(02).
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
     03  HED-ERR             PIC  N(10)  VALUE
                   NC"振替情報ファイル異常".
     03  MEI-ERR             PIC  N(10)  VALUE
                   NC"振替明細ファイル異常".
     03  ZAI-ERR             PIC  N(10)  VALUE
                   NC"在庫マスタ異常".
     03  MES-ERR             PIC  N(10)  VALUE
                   NC"商品名称マスタ異常".
*
*メッセージ出力領域
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  ST-PG           PIC  X(08)  VALUE "SFU3030B".
         05  FILLER          PIC  X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  END-PG          PIC  X(08)  VALUE "SFU3030B".
         05  FILLER          PIC  X(11)  VALUE
                                         " END   *** ".
     03  MSG-OUT1.
         05  MSG-OUT1-FIL1   PIC  X(02)  VALUE "##".
         05  MSG-OUT1-FIL2   PIC  N(11)  VALUE
                             NC"振替ＥＸＣＥＬ読込　＝".
         05  MSG-OUT01       PIC  ZZZ,ZZ9.
         05  MSG-OUT1-FIL3   PIC  X(01)  VALUE " ".
         05  MSG-OUT1-FIL4   PIC  N(01)  VALUE NC"件".
     03  MSG-OUT2.
         05  MSG-OUT2-FIL1   PIC  X(02)  VALUE "##".
         05  MSG-OUT2-FIL2   PIC  N(11)  VALUE
                             NC"振替ファイル作成　　＝".
         05  MSG-OUT02       PIC  ZZZ,ZZ9.
         05  MSG-OUT2-FIL3   PIC  X(01)  VALUE " ".
         05  MSG-OUT2-FIL4   PIC  N(01)  VALUE NC"件".
     03  MSG-OUT3.
         05  MSG-OUT3-FIL1   PIC  X(02)  VALUE "##".
         05  MSG-OUT3-FIL2   PIC  N(11)  VALUE
                             NC"明細ファイル作成　　＝".
         05  MSG-OUT03       PIC  ZZZ,ZZ9.
         05  MSG-OUT3-FIL3   PIC  X(01)  VALUE " ".
         05  MSG-OUT3-FIL4   PIC  N(01)  VALUE NC"件".
     03  MSG-OUT4.
         05  MSG-OUT4-FIL1   PIC  X(02)  VALUE "##".
         05  MSG-OUT4-FIL2   PIC  N(11)  VALUE
                             NC"ヘッダファイル更新　＝".
         05  MSG-OUT04       PIC  ZZZ,ZZ9.
         05  MSG-OUT4-FIL3   PIC  X(01)  VALUE " ".
         05  MSG-OUT4-FIL4   PIC  N(01)  VALUE NC"件".
     03  MSG-OUT5.
         05  MSG-OUT5-FIL1   PIC  X(02)  VALUE "##".
         05  MSG-OUT5-FIL2   PIC  N(11)  VALUE
                             NC"明細ファイル更新　　＝".
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
*伝票番号サブルーチン用ワーク
 01  OUT-DENNO               PIC 9(07).
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
     USE         AFTER       EXCEPTION PROCEDURE SFRWKL1.
     DISPLAY     EXL-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     EXL-ST      UPON      CONS.
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
 ZAI-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE ZAMZAIL1.
     DISPLAY     ZAI-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     ZAI-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 MES-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE MEIMS1.
     DISPLAY     MES-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     MES-ST      UPON      CONS.
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
     OPEN        I-O         SFRWKL1
                             SFRHEDL1  SFRMEIL1
                             ZAMZAIL1  MEIMS1.
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
* 振替ＥＸＣＥＬ読込　終了するまで繰り返す。
     PERFORM  READ-EXL-SEC.
*
     IF       EXL-ENDFLG  =   SPACE
              PERFORM         HENSYU-EXL-SEC
              GO          TO  MAIN-100
     END-IF.
*
     MOVE    "END"        TO  END-FLG.
*
 MAIN-SEC-EXIT.
     EXIT.
****************************************************************
*               振替ＥＸＣＥＬ　順読込
****************************************************************
 READ-EXL-SEC                SECTION.
     MOVE     "READ-EXL-SEC"      TO   S-NAME.
*
     READ     SFRWKL1   AT   END
              MOVE      "Y"       TO   EXL-ENDFLG
              GO                  TO   READ-EXL-EXIT
     END-READ.
*カウント（取込件数）
     ADD      1                   TO   EXL-RD-CNT.
*
 READ-EXL-EXIT.
     EXIT.
***************************************************************
*             振替ＥＸＣＥＬ　チェック編集
***************************************************************
 HENSYU-EXL-SEC             SECTION.
*
     MOVE    "HENSYU-EXL-SEC"      TO   S-NAME.
*
*振替情報ファイル出力
     PERFORM   HED-WRITE-SEC.
*
*振替ＥＸＣＥＬ取込ファイルＲＥＣ削除
     DELETE    SFRWKL1.
*
 HENSYU-EXL-EXIT.
     EXIT.
****************************************************************
*    更新処理（ＩＮＶ、ＩＮＶの場合）Ｈ、Ｍ新規作成
****************************************************************
 KOUSIN1-SEC             SECTION.
*
     MOVE       "KOUSIN1-SEC"     TO   S-NAME.
*ヘッダ部作成
 KOUSIN1-010.
     WRITE       HED-REC.
     ADD         1                TO   HED-WT-CNT.
*明細部セット
 KOUSIN1-020.
     PERFORM     MEI-SET-SEC.
 KOUSIN1-030.
*明細部作成（発注数＝０の場合は明細行を作成しない）
     IF  EXL-F05E2  NOT =  ZERO
         MOVE     EXL-F054  TO  WK-KEY-SOKCD
         MOVE     EXL-F055  TO  WK-KEY-SYOCD
         MOVE     EXL-F056  TO  WK-KEY-HINCD(1:5)
         MOVE     EXL-F057  TO  WK-KEY-HINCD(6:2)
         MOVE     EXL-F058  TO  WK-KEY-HINCD(8:1)
         MOVE     EXL-F05D  TO  WK-KEY-TANBAN
         MOVE     EXL-F05E2 TO  WK-SURYO
         PERFORM  ZAIKO-SEC
**********
         WRITE   MEI-REC
         ADD     1                TO   MEI-WT-CNT
     END-IF.
*
 KOUSIN1-EXIT.
     EXIT.
****************************************************************
*    更新処理（空白、ＩＮＶの場合）明細行追加
****************************************************************
 KOUSIN2-SEC             SECTION.
*
     MOVE       "KOUSIN2-SEC"     TO   S-NAME.
*ヘッダ部を更新
     IF  EXL-F05E1  =  "-"
         COMPUTE    HED-F15  = HED-F15 + ( EXL-F05E2  *  -1 )
     ELSE
         ADD        EXL-F05E2  TO   HED-F15
     END-IF.
*#2018/05/01 NAV ST 更新項目追加
*   商品名
     MOVE   EXL-F05A(1:15)     TO   HED-F09.
     MOVE   EXL-F05A(16:15)    TO   HED-F10.
*   棚番
     MOVE   EXL-F05D           TO   HED-F11.
*   仕入先ＣＤ
     MOVE   EXL-F05C           TO   HED-F12.
*   ＪＡＮＣＤ
     MOVE   EXL-F059           TO   HED-F13.
*   入荷予定日
     MOVE   EXL-F05B           TO   HED-F14.
*   ＭＳＧ
     MOVE   EXL-F05F           TO   HED-F23.
*#2018/05/01 NAV ED 更新項目追加
*   最終発注担当者部門
     MOVE   EXL-F03            TO   HED-F16.
*   最終発注担当者
     MOVE   EXL-F04            TO   HED-F17.
*   最終発注日
     MOVE   EXL-F01            TO   HED-F18.
     REWRITE  HED-REC.
     ADD      1                TO   HED-RW-CNT.
*明細部セット
     PERFORM     MEI-SET-SEC.
*明細部作成（発注数＝０の場合は明細行を作成しない）
     IF  EXL-F05E2  NOT =  ZERO
         MOVE     EXL-F054  TO  WK-KEY-SOKCD
         MOVE     EXL-F055  TO  WK-KEY-SYOCD
         MOVE     EXL-F056  TO  WK-KEY-HINCD(1:5)
         MOVE     EXL-F057  TO  WK-KEY-HINCD(6:2)
         MOVE     EXL-F058  TO  WK-KEY-HINCD(8:1)
         MOVE     EXL-F05D  TO  WK-KEY-TANBAN
         MOVE     EXL-F05E2 TO  WK-SURYO
         PERFORM  ZAIKO-SEC
         WRITE   MEI-REC
         ADD     1                TO   MEI-WT-CNT
     END-IF.
*
 KOUSIN2-EXIT.
     EXIT.
****************************************************************
*    更新処理（空白、空白の場合）Ｈ,Ｍ上書き
****************************************************************
 KOUSIN3-SEC             SECTION.

     MOVE       "KOUSIN3-SEC"     TO   S-NAME.
*明細部数量でヘッダ部発注合計をマイナス
     COMPUTE  HED-F15 = HED-F15 - MEI-F14.
*****在庫更新戻し
     MOVE     HED-F04   TO  WK-KEY-SOKCD.
     MOVE     HED-F05   TO  WK-KEY-SYOCD.
     MOVE     HED-F06   TO  WK-KEY-HINCD(1:5).
     MOVE     HED-F07   TO  WK-KEY-HINCD(6:2).
     MOVE     HED-F08   TO  WK-KEY-HINCD(8:1).
     MOVE     HED-F11   TO  WK-KEY-TANBAN.
     MOVE     MEI-F14   TO  WK-SURYO.
     COMPUTE  WK-SURYO  =  MEI-F14  *  -1.
     PERFORM  ZAIKO-SEC.
*再度、ＥＸＣＥＬ数量で更新する。
     IF  EXL-F05E1  =  "-"
         COMPUTE    HED-F15  = HED-F15 + ( EXL-F05E2  *  -1 )
     ELSE
         ADD        EXL-F05E2  TO   HED-F15
     END-IF.
*#2018/05/01 NAV ST 更新項目追加
*   商品名
     MOVE   EXL-F05A(1:15)     TO   HED-F09.
     MOVE   EXL-F05A(16:15)    TO   HED-F10.
*   棚番
     MOVE   EXL-F05D           TO   HED-F11.
*   仕入先ＣＤ
     MOVE   EXL-F05C           TO   HED-F12.
*   ＪＡＮＣＤ
     MOVE   EXL-F059           TO   HED-F13.
*   入荷予定日
     MOVE   EXL-F05B           TO   HED-F14.
*   ＭＳＧ
     MOVE   EXL-F05F           TO   HED-F23.
*#2018/05/01 NAV ED 更新項目追加
*   最終発注担当者部門
     MOVE   EXL-F03            TO   HED-F16.
*   最終発注担当者
     MOVE   EXL-F04            TO   HED-F17.
*   最終発注日
     MOVE   EXL-F01            TO   HED-F18.
     REWRITE  HED-REC.
     ADD      1                TO   HED-RW-CNT.
*明細部数量をＥＸＣＥＬ数量に置き換える
*   発注／入荷数
     IF     EXL-F05E1  =  "-"
            COMPUTE    MEI-F14  = EXL-F05E2  *  -1
     ELSE
            MOVE       EXL-F05E2  TO   MEI-F14
     END-IF.
*****在庫更新戻し
     MOVE     MEI-F04   TO  WK-KEY-SOKCD.
     MOVE     MEI-F05   TO  WK-KEY-SYOCD.
     MOVE     MEI-F06   TO  WK-KEY-HINCD(1:5).
     MOVE     MEI-F07   TO  WK-KEY-HINCD(6:2).
     MOVE     MEI-F08   TO  WK-KEY-HINCD(8:1).
     MOVE     MEI-F13   TO  WK-KEY-TANBAN.
     MOVE     MEI-F14   TO  WK-SURYO.
     PERFORM  ZAIKO-SEC.
*#2018/05/01 NAV ST 更新項目追加
*   商品名
     MOVE   EXL-F05A(1:15)     TO   MEI-F09.
     MOVE   EXL-F05A(16:15)    TO   MEI-F10.
*   棚番
     MOVE   EXL-F05D           TO   MEI-F13.
*#2018/05/01 NAV ED 更新項目追加
*   ＭＳＧ
     MOVE   EXL-F05F              TO   MEI-F15.
*   更新者部門
     MOVE   EXL-F03               TO   MEI-F96.
*   更新担当者
     MOVE   EXL-F04               TO   MEI-F97.
*   更新日
     MOVE   EXL-F01               TO   MEI-F98.
*   更新時刻
     MOVE   EXL-F02               TO   MEI-F99.
*更新
     REWRITE  MEI-REC.
     ADD      1                   TO   MEI-RW-CNT.
*
 KOUSIN3-EXIT.
     EXIT.
****************************************************************
*    振替情報ファイル出力　　　　　　　　　　　　              *
****************************************************************
 HED-WRITE-SEC           SECTION.
*
     MOVE       "HED-WRITE-SEC"   TO   S-NAME.
*
*振替情報ファイル項目セット
     PERFORM  HED-SET-SEC.
*振替情報ファイル登録
     PERFORM  HED-READ-SEC.
*振替情報明細ファイル登録
     MOVE   EXL-F051            TO        MEI-F01.
     MOVE   EXL-F052            TO        MEI-F02.
     MOVE   EXL-F053            TO        MEI-F03.
     MOVE   EXL-F054            TO        MEI-F04.
     MOVE   EXL-F055            TO        MEI-F05.
     MOVE   EXL-F056            TO        MEI-F06.
     MOVE   EXL-F057            TO        MEI-F07.
     MOVE   EXL-F058            TO        MEI-F08.
     MOVE   "1"                 TO        MEI-F11.
*****MOVE   EXL-F01             TO        MEI-F12.
     MOVE   EXL-F05B            TO        MEI-F12.
     PERFORM  MEI-READ-SEC.
*更新判定
*****新規取込
*    DISPLAY "HED-F01     = " HED-F01     UPON  CONS.
*    DISPLAY "HED-F02     = " HED-F02     UPON  CONS.
*    DISPLAY "HED-F03     = " HED-F03     UPON  CONS.
*    DISPLAY "HED-F04     = " HED-F04     UPON  CONS.
*    DISPLAY "HED-F05     = " HED-F05     UPON  CONS.
*    DISPLAY "HED-F06     = " HED-F06     UPON  CONS.
*    DISPLAY "HED-F07     = " HED-F07     UPON  CONS.
*    DISPLAY "HED-F08     = " HED-F08     UPON  CONS.
*    DISPLAY "MEI-F01     = " MEI-F01     UPON  CONS.
*    DISPLAY "MEI-F02     = " MEI-F02     UPON  CONS.
*    DISPLAY "MEI-F03     = " MEI-F03     UPON  CONS.
*    DISPLAY "MEI-F04     = " MEI-F04     UPON  CONS.
*    DISPLAY "MEI-F05     = " MEI-F05     UPON  CONS.
*    DISPLAY "MEI-F06     = " MEI-F06     UPON  CONS.
*    DISPLAY "MEI-F07     = " MEI-F07     UPON  CONS.
*    DISPLAY "MEI-F08     = " MEI-F08     UPON  CONS.
*    DISPLAY "MEI-F11     = " MEI-F11     UPON  CONS.
*    DISPLAY "MEI-F12     = " MEI-F12     UPON  CONS.
*    DISPLAY "HED-INV-FLG = " HED-INV-FLG UPON  CONS.
*    DISPLAY "MEI-INV-FLG = " MEI-INV-FLG UPON  CONS.
     IF  HED-INV-FLG = "INV"
     AND MEI-INV-FLG = "INV"
         PERFORM  KOUSIN1-SEC
     END-IF.
     IF  HED-INV-FLG = SPACE
     AND MEI-INV-FLG = "INV"
         PERFORM  KOUSIN2-SEC
     END-IF.
     IF  HED-INV-FLG = SPACE
     AND MEI-INV-FLG = SPACE
         PERFORM  KOUSIN3-SEC
     END-IF.
*
 HED-WRITE-EXIT.
     EXIT.
****************************************************************
*               振替情報ファイル項目セット                     *
****************************************************************
 HED-SET-SEC            SECTION.
*
     MOVE  "HED-SET-SEC"          TO   S-NAME.
*
     MOVE   SPACE                 TO   HED-REC.
     INITIALIZE                        HED-REC.
*   伝票区分
     MOVE   EXL-F051              TO   HED-F01.
*   年度
     MOVE   EXL-F052              TO   HED-F02.
*   シーズン
     MOVE   EXL-F053              TO   HED-F03.
*   倉庫ＣＤ
     MOVE   EXL-F054              TO   HED-F04.
*   サカタ商品ＣＤ
     MOVE   EXL-F055              TO   HED-F05.
*   品単ＣＤ
     MOVE   EXL-F056              TO   HED-F06.
     MOVE   EXL-F057              TO   HED-F07.
     MOVE   EXL-F058              TO   HED-F08.
*   商品名
     MOVE   EXL-F05A(1:15)        TO   HED-F09.
     MOVE   EXL-F05A(16:15)       TO   HED-F10.
*   棚番
     MOVE   EXL-F05D              TO   HED-F11.
*   仕入先ＣＤ
     MOVE   EXL-F05C              TO   HED-F12.
*   ＪＡＮＣＤ
     MOVE   EXL-F059              TO   HED-F13.
*   入荷予定日
     MOVE   EXL-F05B              TO   HED-F14.
*   発注数合計
     IF     EXL-F05E1  =  "-"
            COMPUTE    HED-F15  = EXL-F05E2  *  -1
     ELSE
            MOVE       EXL-F05E2  TO   HED-F15
     END-IF.
*   最終発注担当者部門
     MOVE   EXL-F03               TO   HED-F16.
*   最終発注担当者
     MOVE   EXL-F04               TO   HED-F17.
*   最終発注日
     MOVE   EXL-F01               TO   HED-F18.
*   入荷数合計:INITIAL
*   最終入荷担当者部門:INITIAL
*   最終入荷担当者　　:INITIAL
*   最終入荷日　　　　:INITIAL
*   ＭＳＧ
     MOVE   EXL-F05F              TO   HED-F23.
*   完納区分　　　　　:INITIAL
*   完納日　　　　　　:INITIAL
*   完納者部門　　　　:INITIAL
*   完納担当者　　　　:INITIAL
*   予備領域　　　　　:INITIAL
*   入力区分
     MOVE   "1"                   TO   HED-F91.
*   登録者部門
     MOVE   EXL-F03               TO   HED-F92.
*   登録担当者
     MOVE   EXL-F04               TO   HED-F93.
*   登録日
     MOVE   EXL-F01               TO   HED-F94.
*   登録時刻
     MOVE   EXL-F02               TO   HED-F95.
*   更新者部門        :INITIAL
     MOVE   EXL-F03               TO   HED-F96.
*   更新担当者        :INITIAL
     MOVE   EXL-F04               TO   HED-F97.
*   更新日            :INITIAL
     MOVE   EXL-F01               TO   HED-F98.
*   更新時刻          :INITIAL
     MOVE   EXL-F02               TO   HED-F99.
*
 HED-SET-EXIT.
     EXIT.
****************************************************************
*       振替明細ファイル出力
****************************************************************
 MEI-SET-SEC                 SECTION.
*
     MOVE    "MEI-SET-SEC"      TO        S-NAME.
*
     MOVE     SPACE             TO        MEI-REC.
     INITIALIZE                           MEI-REC.
*
*   伝票区分
     MOVE   EXL-F051            TO        MEI-F01.
*   年度
     MOVE   EXL-F052            TO        MEI-F02.
*   シーズン
     MOVE   EXL-F053            TO        MEI-F03.
*   倉庫ＣＤ
     MOVE   EXL-F054            TO        MEI-F04.
*   サカタ商品ＣＤ
     MOVE   EXL-F055            TO        MEI-F05.
*   品単ＣＤ
     MOVE   EXL-F056            TO        MEI-F06.
     MOVE   EXL-F057            TO        MEI-F07.
     MOVE   EXL-F058            TO        MEI-F08.
*   商品名
     MOVE   EXL-F05A(1:15)      TO        MEI-F09.
     MOVE   EXL-F05A(16:15)     TO        MEI-F10.
*   発注入荷区分
     MOVE   "1"                 TO        MEI-F11.
*   発注日／入荷日
*****MOVE   EXL-F01             TO        MEI-F12.
     MOVE   EXL-F05B            TO        MEI-F12.
*   棚番　　　
     MOVE   EXL-F05D            TO        MEI-F13.
*   発注／入荷数
     IF     EXL-F05E1  =  "-"
            COMPUTE    MEI-F14  = EXL-F05E2  *  -1
     ELSE
            MOVE       EXL-F05E2  TO   MEI-F14
     END-IF.
*   ＭＳＧ
     MOVE   EXL-F05F              TO   MEI-F15.
*   予備領域　　　　　:INITIAL
*   入力区分
     MOVE   "1"                   TO   MEI-F91.
*   登録者部門
     MOVE   EXL-F03               TO   MEI-F92.
*   登録担当者
     MOVE   EXL-F04               TO   MEI-F93.
*   登録日
     MOVE   EXL-F01               TO   MEI-F94.
*   登録時刻
     MOVE   EXL-F02               TO   MEI-F95.
*   更新者部門
     MOVE   EXL-F03               TO   MEI-F96.
*   更新担当者
     MOVE   EXL-F04               TO   MEI-F97.
*   更新日
     MOVE   EXL-F01               TO   MEI-F98.
*   更新時刻
     MOVE   EXL-F02               TO   MEI-F99.
*
 MEI-SET-EXIT.
     EXIT.
****************************************************************
*    振替情報ファイル読込
****************************************************************
 HED-READ-SEC              SECTION.
*
     MOVE "HED-READ-SEC"    TO    S-NAME.
*
     READ     SFRHEDL1
       INVALID
              MOVE "INV"     TO   HED-INV-FLG
       NOT  INVALID
              MOVE SPACE     TO   HED-INV-FLG
     END-READ.
*
 HED-READ-EXIT.
     EXIT.
****************************************************************
*    振替情報明細ファイル読込
****************************************************************
 MEI-READ-SEC              SECTION.
*
     MOVE "MEI-READ-SEC"    TO    S-NAME.
*
     READ     SFRMEIL1
       INVALID
              MOVE "INV"     TO   MEI-INV-FLG
       NOT  INVALID
              MOVE SPACE     TO   MEI-INV-FLG
     END-READ.
*
 MEI-READ-EXIT.
     EXIT.
****************************************************************
*    終了
****************************************************************
 END-SEC                   SECTION.
*
     MOVE     "END-SEC"     TO    S-NAME.
*
     MOVE      EXL-RD-CNT   TO    MSG-OUT01.
     MOVE      HED-WT-CNT   TO    MSG-OUT02.
     MOVE      MEI-WT-CNT   TO    MSG-OUT03.
     MOVE      HED-RW-CNT   TO    MSG-OUT04.
     MOVE      MEI-RW-CNT   TO    MSG-OUT05.
     DISPLAY   MSG-OUT1-FIL1 MSG-OUT1-FIL2 MSG-OUT01
               MSG-OUT1-FIL3 MSG-OUT1-FIL4 UPON CONS.
     DISPLAY   MSG-OUT2-FIL1 MSG-OUT2-FIL2 MSG-OUT02
               MSG-OUT2-FIL3 MSG-OUT2-FIL4 UPON CONS.
     DISPLAY   MSG-OUT3-FIL1 MSG-OUT3-FIL2 MSG-OUT03
               MSG-OUT3-FIL3 MSG-OUT3-FIL4 UPON CONS.
     DISPLAY   MSG-OUT4-FIL1 MSG-OUT4-FIL2 MSG-OUT04
               MSG-OUT4-FIL3 MSG-OUT4-FIL4 UPON CONS.
     DISPLAY   MSG-OUT5-FIL1 MSG-OUT5-FIL2 MSG-OUT04
               MSG-OUT5-FIL3 MSG-OUT5-FIL4 UPON CONS.
*
     CLOSE     SFRWKL1
               SFRMEIL1
               SFRHEDL1
               ZAMZAIL1
               MEIMS1.
*
     DISPLAY   MSG-END      UPON  CONS.
*
 END-EXIT.
     EXIT.
****************************************************************
*    在庫更新処理
****************************************************************
 ZAIKO-SEC                  SECTION.
*在庫マスタを索引する
     MOVE    WK-KEY-SOKCD   TO   ZAI-F01.
     MOVE    WK-KEY-SYOCD   TO   ZAI-F021.
     MOVE    WK-KEY-HINCD   TO   ZAI-F022.
     MOVE    WK-KEY-TANBAN  TO   ZAI-F03.
     READ    ZAMZAIL1
             INVALID
             PERFORM   ZAIKO-WRITE-SEC
             NOT  INVALID
             PERFORM   ZAIKO-REWRITE-SEC
     END-READ.
*
 ZAIKO-EXIT.
     EXIT.
****************************************************************
*    在庫更新処理１
****************************************************************
 ZAIKO-WRITE-SEC            SECTION.
*商品名称マスタ索引
     MOVE    WK-KEY-SYOCD   TO   MES-F011.
     MOVE    WK-KEY-HINCD   TO   MES-F012.
     PERFORM  HMEIMS-READ-SEC.
*在庫マスタレコードを初期化
     MOVE    SPACE          TO   ZAI-REC.
     INITIALIZE                  ZAI-REC.
*キー部セット
     MOVE    WK-KEY-SOKCD   TO   ZAI-F01.
     MOVE    WK-KEY-SYOCD   TO   ZAI-F021.
     MOVE    WK-KEY-HINCD   TO   ZAI-F022.
     MOVE    WK-KEY-TANBAN  TO   ZAI-F03.
*****未入庫数
     COMPUTE ZAI-F26  =  ZAI-F26  +  WK-SURYO.
*商品名カナセット
     IF   HMEIMS-INV-FLG =  SPACE
          MOVE  MES-F031    TO   ZAI-F30
     ELSE
          MOVE  SPACE       TO   ZAI-F30
     END-IF.
*登録日／更新日セット
     MOVE WK-DATE8          TO   ZAI-F98.
     MOVE WK-DATE8          TO   ZAI-F99.
*
     WRITE  ZAI-REC.
*
 ZAIKO-WRITE-EXIT.
     EXIT.
****************************************************************
*    在庫更新処理２
****************************************************************
 ZAIKO-REWRITE-SEC          SECTION.
*
*****未入庫数
     COMPUTE ZAI-F26  =  ZAI-F26  +  WK-SURYO.
*更新日セット
     MOVE WK-DATE8          TO   ZAI-F99.
*
     REWRITE  ZAI-REC.
*
 ZAIKO-REWRITE-EXIT.
     EXIT.
****************************************************************
*      ALL        商品名称マスタ読込　　　　　                 *
****************************************************************
 HMEIMS-READ-SEC        SECTION.
*
     READ     MEIMS1
              INVALID     MOVE  "INV"    TO  HMEIMS-INV-FLG
              NOT INVALID MOVE  SPACE    TO  HMEIMS-INV-FLG
     END-READ.
*
 HMEIMS-READ-EXIT.
     EXIT.

```
