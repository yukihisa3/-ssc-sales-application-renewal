****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　検品システム　　　　　　　　　　　*
*    モジュール名　　　　：　返品検品データ取込更新　　　　　　*
*    作成日／作成者　　　：　2019/01/30 T.TAKAHASHI            *
*    処理内容　　　　　　：　倉庫返品管理データを読み、ホスト　*
*    　　　　　　　　　　　　返品関係Ｆを作成する。　　　　　　*
*    変更日／作成者　　　：　                                  *
*    変更内容　　　　　　：　                                  *
****************************************************************
 IDENTIFICATION              DIVISION.
 PROGRAM-ID.                 NKE0310B.
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
*返品管理ファイル
     SELECT      RCVHHDXX    ASSIGN    TO       DA-01-S-RCVHHDXX
                             ACCESS    MODE     SEQUENTIAL
                             FILE      STATUS   HHD-ST.
*返品管理明細ファイル
     SELECT      RCVHHMXX    ASSIGN    TO       DA-01-S-RCVHHMXX
                             ACCESS    MODE     SEQUENTIAL
                             FILE      STATUS   HHM-ST.
*返品実績ヘッダファイル
     SELECT      RCVHJHXX    ASSIGN    TO       DA-01-S-RCVHJHXX
                             ACCESS    MODE     SEQUENTIAL
                             FILE      STATUS   HJH-ST.
*返品実績明細ファイル
     SELECT      RCVHJMXX    ASSIGN    TO       DA-01-S-RCVHJMXX
                             ACCESS    MODE     SEQUENTIAL
                             FILE      STATUS   HJM-ST.
*ホスト返品管理ファイル
     SELECT      HENKANF   ASSIGN    TO         DA-01-VI-HENKANL2
                           ORGANIZATION         INDEXED
                           ACCESS    MODE       DYNAMIC
                           RECORD    KEY        KAN-F01
                                                KAN-F98
                                                KAN-F99
                                                KAN-F02
                                                KAN-F03
                                                KAN-F04
                                                KAN-F05
                           FILE      STATUS     KAN-ST.
*
*ホスト返品管理明細ファイル
     SELECT      HENKAMF     ASSIGN    TO       DA-01-VI-HENKAML2
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     DYNAMIC
                             RECORD    KEY      KAM-F01
                                                KAM-F98
                                                KAM-F99
                                                KAM-F02
                                                KAM-F03
                                                KAM-F04
                                                KAM-F05
                                                KAM-F06
                             FILE      STATUS   KAM-ST.
*
*ホスト返品実績ファイル
     SELECT      HENKJSF     ASSIGN    TO       DA-01-VI-HENKJSL2
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     DYNAMIC
                             RECORD    KEY      KJS-F01
                                                KJS-F98
                                                KJS-F99
                                                KJS-F02
                                                KJS-F03
                                                KJS-F04
                                                KJS-F05
                                                KJS-F06
                                                KJS-F11
                                                KJS-F12
                                                KJS-F13
                             FILE      STATUS   KJS-ST.
*担当者変換マスタ
     SELECT      TANHENL1    ASSIGN    TO       DA-01-VI-TANHENL1
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     RANDOM
                             RECORD    KEY      TAN-F01
                             FILE      STATUS   TAN-ST.
*
****************************************************************
 DATA                        DIVISION.
****************************************************************
 FILE                        SECTION.
*返品管理ファイル
 FD  RCVHHDXX            LABEL RECORD   IS   STANDARD
     BLOCK               CONTAINS       55   RECORDS.
     COPY        RCVHHDF                OF   XFDLIB
                 JOINING     HHD        AS   PREFIX.
*返品管理明細ファイル
 FD  RCVHHMXX            LABEL RECORD   IS   STANDARD
     BLOCK               CONTAINS       90   RECORDS.
     COPY        RCVHHMF                OF   XFDLIB
                 JOINING     HHM        AS   PREFIX.
*返品実績ヘッダファイル
 FD  RCVHJHXX            LABEL RECORD   IS   STANDARD
     BLOCK               CONTAINS       45   RECORDS.
     COPY        RCVHJHF                OF   XFDLIB
                 JOINING     HJH        AS   PREFIX.
*返品実績明細ファイル
 FD  RCVHJMXX            LABEL RECORD   IS   STANDARD
     BLOCK               CONTAINS       85   RECORDS.
     COPY        RCVHJMF                OF   XFDLIB
                 JOINING     HJM        AS   PREFIX.
*ホスト返品管理ファイル
 FD  HENKANF.
     COPY        HENKANF     OF        XFDLIB
     JOINING     KAN         AS        PREFIX.
*ホスト返品管理明細ファイル
 FD  HENKAMF.
     COPY        HENKAMF     OF        XFDLIB
     JOINING     KAM         AS        PREFIX.
*ホスト返品実績ファイル
 FD  HENKJSF.
     COPY        HENKJSF     OF        XFDLIB
     JOINING     KJS         AS        PREFIX.
*担当者変換マスタ
 FD  TANHENL1.
     COPY        TANHENL1    OF        XFDLIB
     JOINING     TAN         AS        PREFIX.
****************************************************************
 WORKING-STORAGE           SECTION.
****************************************************************
 01  ST-AREA.
     03  HHD-ST              PIC  X(02)  VALUE  SPACE.
     03  HHM-ST              PIC  X(02)  VALUE  SPACE.
     03  HJH-ST              PIC  X(02)  VALUE  SPACE.
     03  HJM-ST              PIC  X(02)  VALUE  SPACE.
     03  KAN-ST              PIC  X(02)  VALUE  SPACE.
     03  KAM-ST              PIC  X(02)  VALUE  SPACE.
     03  KJS-ST              PIC  X(02)  VALUE  SPACE.
     03  TAN-ST              PIC  X(02)  VALUE  SPACE.
 01  SV-AREA.
     03  BR-KAN-F01          PIC  9(07)  VALUE  ZERO.
 01  WK-AREA.
     03  DPNO                PIC  9(07)  VALUE  ZERO.
     03  I                   PIC  9(02)  VALUE  ZERO.
     03  END-FLG             PIC  X(03)  VALUE  SPACE.
     03  EXL-ENDFLG          PIC  X(01)  VALUE  SPACE.
     03  RD-CNT              PIC  9(07)  VALUE  ZERO.
     03  KAN-CNT             PIC  9(07)  VALUE  ZERO.
     03  SKIP-CNT            PIC  9(07)  VALUE  ZERO.
     03  NYK-CNT             PIC  9(07)  VALUE  ZERO.
     03  HED-CNT             PIC  9(07)  VALUE  ZERO.
     03  MEI-CNT             PIC  9(07)  VALUE  ZERO.
     03  ERR-CNT             PIC  9(07)  VALUE  ZERO.
     03  WK-NYUKA-CHK        PIC  9(07)  VALUE  ZERO.
     03  ERR-KBN             PIC  X(01)  VALUE  SPACE.
**
 01  RCVHHDXX-END-FLG        PIC  X(03)  VALUE  SPACE.
 01  RCVHHMXX-END-FLG        PIC  X(03)  VALUE  SPACE.
 01  RCVHJHXX-END-FLG        PIC  X(03)  VALUE  SPACE.
 01  RCVHJMXX-END-FLG        PIC  X(03)  VALUE  SPACE.
*
 01  HENKANL2-INV-FLG        PIC  X(03)  VALUE  SPACE.
 01  HENKAML2-INV-FLG        PIC  X(03)  VALUE  SPACE.
 01  HENKJSL2-INV-FLG        PIC  X(03)  VALUE  SPACE.
 01  TANHENL1-INV-FLG        PIC  X(03)  VALUE  SPACE.
*
 01  RCVHHDXX-IN-CNT         PIC  9(07)  VALUE  ZERO.
 01  RCVHHDXX-SKIP-CNT       PIC  9(07)  VALUE  ZERO.
 01  HENKANL2-OUT-CNT        PIC  9(07)  VALUE  ZERO.
*
 01  RCVHHMXX-IN-CNT         PIC  9(07)  VALUE  ZERO.
 01  RCVHHMXX-SKIP-CNT       PIC  9(07)  VALUE  ZERO.
 01  HENKAML2-OUT-CNT        PIC  9(07)  VALUE  ZERO.
*
 01  RCVHJHXX-IN-CNT         PIC  9(07)  VALUE  ZERO.
 01  RCVHJMXX-IN-CNT         PIC  9(07)  VALUE  ZERO.
 01  RCVHJMXX-SKIP-CNT       PIC  9(07)  VALUE  ZERO.
 01  HENKJSL2-OUT-CNT        PIC  9(07)  VALUE  ZERO.
*
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
***  エラーセクション名
 01  SEC-NAME.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-SEC    => ".
     03  S-NAME                   PIC  X(20).
*
 01  FILE-ERR.
     03  HHD-ERR            PIC  N(10)  VALUE
                   NC"返品管理Ｆ　　　異常".
     03  HHM-ERR             PIC  N(10)  VALUE
                   NC"返品管理明細Ｆ　異常".
     03  HJH-ERR             PIC  N(10)  VALUE
                   NC"返品実績ヘッダＦ異常".
     03  HJM-ERR             PIC  N(10)  VALUE
                   NC"返品実績明細Ｆ　異常".
     03  KAN-ERR             PIC  N(10)  VALUE
                   NC"ホスト返品管理Ｆ異常".
     03  KAM-ERR             PIC  N(10)  VALUE
                   NC"ホスト返品明細Ｆ異常".
     03  KJS-ERR             PIC  N(10)  VALUE
                   NC"ホスト返品実績Ｆ異常".
*
*メッセージ出力領域
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  ST-PG           PIC  X(08)  VALUE "NKE0310B".
         05  FILLER          PIC  X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  END-PG          PIC  X(08)  VALUE "NKE0310B".
         05  FILLER          PIC  X(11)  VALUE
                                         " END   *** ".
*日付編集用
 01  WK-HHD-F04              PIC  9(08).
 01  FILLER                  REDEFINES  WK-HHD-F04.
     03  WK-HHD-F04-1        PIC  X(04).
     03  WK-HHD-F04-2        PIC  X(02).
     03  WK-HHD-F04-3        PIC  X(02).
 01  WK-HHD-F08              PIC  9(08).
 01  FILLER                  REDEFINES  WK-HHD-F08.
     03  WK-HHD-F08-1        PIC  X(04).
     03  WK-HHD-F08-2        PIC  X(02).
     03  WK-HHD-F08-3        PIC  X(02).
 01  WK-HHM-F04              PIC  9(08).
 01  FILLER                  REDEFINES  WK-HHM-F04.
     03  WK-HHM-F04-1        PIC  X(04).
     03  WK-HHM-F04-2        PIC  X(02).
     03  WK-HHM-F04-3        PIC  X(02).
 01  WK-HJH-F04              PIC  9(08).
 01  FILLER                  REDEFINES  WK-HJH-F04.
     03  WK-HJH-F04-1        PIC  X(04).
     03  WK-HJH-F04-2        PIC  X(02).
     03  WK-HJH-F04-3        PIC  X(02).
 01  WK-HJH-F07              PIC  9(08).
 01  FILLER                  REDEFINES  WK-HJH-F07.
     03  WK-HJH-F07-1        PIC  X(04).
     03  WK-HJH-F07-2        PIC  X(02).
     03  WK-HJH-F07-3        PIC  X(02).
*時刻編集用
 01  WK-HHD-F09              PIC  9(06).
 01  FILLER                  REDEFINES  WK-HHD-F09.
     03  WK-HHD-F09-1        PIC  X(02).
     03  WK-HHD-F09-2        PIC  X(02).
     03  WK-HHD-F09-3        PIC  X(02).
 01  WK-HJH-F10              PIC  9(06).
 01  FILLER                  REDEFINES  WK-HJH-F10.
     03  WK-HJH-F10-1        PIC  X(02).
     03  WK-HJH-F10-2        PIC  X(02).
     03  WK-HJH-F10-3        PIC  X(02).
 01  WK-HJH-F11              PIC  9(06).
 01  FILLER                  REDEFINES  WK-HJH-F11.
     03  WK-HJH-F11-1        PIC  X(02).
     03  WK-HJH-F11-2        PIC  X(02).
     03  WK-HJH-F11-3        PIC  X(02).
*
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN             PIC X(01).
 01  LINK-IN-YMD6            PIC 9(06).
 01  LINK-IN-YMD8            PIC 9(08).
 01  LINK-OUT-RET            PIC X(01).
 01  LINK-OUT-YMD            PIC 9(08).
*伝票番号サブルーチン用ワーク
*01  OUT-DENNO               PIC 9(07).
****************************************************************
 LINKAGE                     SECTION.
****************************************************************
 01  LINK-IN-BUMON               PIC  X(04).
 01  LINK-IN-TANCD               PIC  X(02).
 01  LINK-IN-SOKCD               PIC  X(02).
 01  LINK-IN-TDATE               PIC  9(08).
 01  LINK-IN-TTIME               PIC  9(06).
****************************************************************
 PROCEDURE                   DIVISION  USING LINK-IN-BUMON
                                             LINK-IN-TANCD
                                             LINK-IN-SOKCD
                                             LINK-IN-TDATE
                                             LINK-IN-TTIME.
****************************************************************
 DECLARATIVES.
 HHD-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE RCVHHDXX.
     DISPLAY     HHD-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     HHD-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 HHM-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE RCVHHMXX.
     DISPLAY     HHM-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     HHM-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 HJH-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE RCVHJHXX.
     DISPLAY     HJH-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     HJH-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 HJM-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE RCVHJMXX.
     DISPLAY     HJM-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     HJM-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 KAN-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE HENKANF.
     DISPLAY     KAN-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     KAN-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 KAM-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE HENKAMF.
     DISPLAY     KAM-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     KAM-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 KJS-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE HENKJSF.
     DISPLAY     KJS-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     KJS-ST      UPON      CONS.
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
     OPEN        I-O         HENKANF   HENKAMF   HENKJSF.
     OPEN        INPUT       RCVHHDXX  RCVHHMXX
                             RCVHJHXX  RCVHJMXX  TANHENL1.
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
*在庫締め日取得
*    MOVE      99                 TO   JYO-F01.
*    MOVE     "ZAI"               TO   JYO-F02.
*    PERFORM  JYOKEN1-READ-SEC.
*    IF   JYOKEN1-INV-FLG = "INV"
*         DISPLAY NC"＃＃条件Ｆ存在無（在庫締日）＃＃" UPON CONS
*         MOVE    4000            TO   PROGRAM-STATUS
*         STOP    RUN
*    ELSE
*         MOVE    JYO-F04         TO   ZAI-SIME1
*         DISPLAY "# " NC"経理月" " = " ZAI-SIME1R1 " #"
*                 UPON CONS
*    END-IF.
*返品管理ファイル読込
     PERFORM  RCVHHDXX-READ-SEC.
     IF  RCVHHDXX-END-FLG = "END"
          DISPLAY NC"＃＃返品管理ファイルに＃＃" UPON CONS
          DISPLAY NC"＃＃対象データ無し！！＃＃" UPON CONS
          MOVE    "END"           TO   END-FLG
          MOVE    4000            TO   PROGRAM-STATUS
     END-IF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*                 M A I N - S E C
****************************************************************
 MAIN-SEC                    SECTION.
     MOVE     "MAIN-SEC"          TO   S-NAME.
* 初期化
*    MOVE     SPACE               TO   ERR-KBN.
*    MOVE     ZERO                TO   WK-END-EDA.
* _ ホスト返品管理ファイル出力
*    RCVHHDXX -> HENKANL2
     PERFORM  HENKANL2-OUT-SEC
              UNTIL   RCVHHDXX-END-FLG  =  "END".
*
* _ ホスト返品管理明細ファイル出力
*    RCVHHMXX -> HENKAML2
     PERFORM  HENKAML2-OUT-SEC
              UNTIL   RCVHHMXX-END-FLG  =  "END".
*
* _ ホスト返品実績ファイル出力
*    RCVHJHXX & RCVHJMXX -> HENKAML2
     PERFORM  HENKJSL2-OUT-SEC
              UNTIL ( RCVHJHXX-END-FLG  =  "END" )
                 OR ( RCVHJMXX-END-FLG  =  "END" ).
*
* 各マスタ、ファイルチェック
*    発注ヘッダ索引
*    MOVE     HHD-F01             TO   HED-F02.
*    PERFORM  HENKANL2-READ-SEC.
*****DISPLAY "HENKANL2-INV-FLG = " HENKANL2-INV-FLG UPON CONS.
*    IF  HENKANL2-INV-FLG  =  "INV"
*             MOVE  "1"           TO   KAN-F23 ERR-KBN
*    ELSE
*             MOVE  HED-F33       TO   WK-END-EDA
*    END-IF.
*    発注明細索引
*    MOVE     HHD-F01             TO   MEI-A01.
*    MOVE     HHD-F02             TO   MEI-A02.
*****DISPLAY "MEI-A01 = " MEI-A01 UPON CONS.
*****DISPLAY "MEI-A02 = " MEI-A02 UPON CONS.
*    PERFORM  HENKAML2-READ-SEC.
*****DISPLAY "HENKAML2-INV-FLG = " HENKAML2-INV-FLG UPON CONS.
*    IF  HENKAML2-INV-FLG  =  "INV"
*             MOVE  "1"           TO   KAN-F24 ERR-KBN
**************DISPLAY "AAA" UPON CONS
*    ELSE
**************DISPLAY "BBB" UPON CONS
*        既に完了済みの場合
*        IF  MEI-F05 = 1 OR 9
*            MOVE  "1"           TO   KAN-F25 ERR-KBN
*        ELSE
**************MOVE  "1"           TO   KAN-F25 ERR-KBN
*             返品数を超えているか？
*             COMPUTE WK-NYUKA-CHK = MEI-F10  +  HHD-F14
*             IF  WK-NYUKA-CHK > MEI-F09
*                 MOVE  "1"       TO   KAN-F26 ERR-KBN
*             END-IF
*        END-IF
*    END-IF.
*    商品名称マスタ索引
*    MOVE     HHD-F06             TO   MES-F011.
*    MOVE     HHD-F07             TO   MES-F0121.
*    MOVE     HHD-F08             TO   MES-F0122.
*    MOVE     HHD-F09             TO   MES-F0123.
*    PERFORM  MEIMS1-READ-SEC.
*    IF  MEIMS1-INV-FLG  =  "INV"
*             MOVE  "1"           TO   KAN-F27 ERR-KBN
*    END-IF.
*    仕入先マスタ索引
*    MOVE     HHD-F03             TO   SHI-F01.
*    PERFORM  ZSHIMS1-READ-SEC.
*    IF  ZSHIMS1-INV-FLG  =  "INV"
*             MOVE  "1"           TO   KAN-F28 ERR-KBN
*    END-IF.
*    倉庫マスタ索引
*    MOVE     LINK-IN-SOKCD       TO   SOK-F01.
*    PERFORM  ZSOKMS1-READ-SEC.
*    IF  ZSOKMS1-INV-FLG  =  "INV"
*             MOVE  "1"           TO   KAN-F29 ERR-KBN
*    END-IF.
*    エラー判定
*    IF  ERR-KBN = "1"
*        MOVE      "1"            TO   KAN-F22
*        ADD        1             TO   ERR-CNT
*********発注ヘッダワーク更新
*        MOVE      "1"            TO   HED-F41
*        REWRITE  HED-REC
*        ADD        1             TO   HED-CNT
*        MOVE      "1"            TO   MEI-A03
*        MOVE    HHD-F01          TO   MEI-B01
*        MOVE    HHD-F02          TO   MEI-B02
*        MOVE    HHD-F03          TO   MEI-B03
*        MOVE    HHD-F04          TO   MEI-B04
*        MOVE    HHD-F05          TO   MEI-B05
*        MOVE    HHD-F06          TO   MEI-B06(1:8)
*        MOVE    HHD-F07          TO   MEI-B06(9:5)
*        MOVE    HHD-F08          TO   MEI-B06(14:2)
*        MOVE    HHD-F09          TO   MEI-B06(16:1)
*        MOVE    HHD-F10          TO   MEI-B07
*        MOVE    HHD-F11          TO   MEI-B08
*        MOVE    HHD-F12          TO   MEI-B09
*        MOVE    HHD-F13          TO   MEI-B10
*        MOVE    HHD-F14          TO   MEI-B11
*        MOVE    HHD-F15          TO   MEI-B12
*        MOVE    HHD-F16          TO   MEI-B13
*        MOVE    HHD-F17          TO   MEI-B14
*        MOVE    HHD-F18          TO   MEI-B15
*        MOVE    HHD-F19          TO   MEI-B16
*        MOVE    HHD-F20          TO   MEI-B17
*        REWRITE MEI-REC
*        ADD     1                TO   MEI-CNT
*    ELSE
*        MOVE    HHD-F01          TO   MEI-B01
*        MOVE    HHD-F02          TO   MEI-B02
*        MOVE    HHD-F03          TO   MEI-B03
*        MOVE    HHD-F04          TO   MEI-B04
*        MOVE    HHD-F05          TO   MEI-B05
*        MOVE    HHD-F06          TO   MEI-B06(1:8)
*        MOVE    HHD-F07          TO   MEI-B06(9:5)
*        MOVE    HHD-F08          TO   MEI-B06(14:2)
*        MOVE    HHD-F09          TO   MEI-B06(16:1)
*        MOVE    HHD-F10          TO   MEI-B07
*        MOVE    HHD-F11          TO   MEI-B08
*        MOVE    HHD-F12          TO   MEI-B09
*        MOVE    HHD-F13          TO   MEI-B10
*        MOVE    HHD-F14          TO   MEI-B11
*        MOVE    HHD-F15          TO   MEI-B12
*        MOVE    HHD-F16          TO   MEI-B13
*        MOVE    HHD-F17          TO   MEI-B14
*        MOVE    HHD-F18          TO   MEI-B15
*        MOVE    HHD-F19          TO   MEI-B16
*        MOVE    HHD-F20          TO   MEI-B17
*        REWRITE MEI-REC
*        ADD     1                TO   MEI-CNT
*    END-IF.
*****倉庫返品検品確定累積Ｆ更新
*    WRITE   KAN-REC.
*    ADD     1                    TO   KAN-CNT.
*
*    PERFORM  RCVHHDXX-READ-SEC.
*
     MOVE   "END"                 TO   END-FLG.
*
 MAIN-SEC-EXIT.
     EXIT.
****************************************************************
*    ホスト返品管理ファイル出力
****************************************************************
 HENKANL2-OUT-SEC             SECTION.
     MOVE "HENKANL2-OUT-SEC"      TO   S-NAME.
*
*返品日　数値化
     MOVE  HHD-F04(1:4)           TO   WK-HHD-F04-1.
     MOVE  HHD-F04(6:2)           TO   WK-HHD-F04-2.
     MOVE  HHD-F04(9:2)           TO   WK-HHD-F04-3.
*受付　　　数値化
     MOVE  HHD-F08(1:4)           TO   WK-HHD-F08-1.
     MOVE  HHD-F08(6:2)           TO   WK-HHD-F08-2.
     MOVE  HHD-F08(9:2)           TO   WK-HHD-F08-3.
*受付時刻　数値化
     MOVE  HHD-F09(1:2)           TO   WK-HHD-F09-1.
     MOVE  HHD-F09(4:2)           TO   WK-HHD-F09-2.
     MOVE  HHD-F09(7:2)           TO   WK-HHD-F09-3.
*
*存在チェック
     PERFORM  HENKANL2-READ-SEC.
     IF       HENKANL2-INV-FLG =  "   "
              ADD    1            TO   RCVHHDXX-SKIP-CNT
              GO                  TO   HENKANL2-OUT-99
     END-IF.
*
     MOVE  SPACE                  TO   KAN-REC.
     INITIALIZE                        KAN-REC.
*
*倉庫ＣＤ
     MOVE  LINK-IN-SOKCD          TO   KAN-F01.
*返品グループ番号
     MOVE  HHD-F01                TO   KAN-F02.
*取引先ＣＤ
     MOVE  HHD-F02                TO   KAN-F03.
*店舗ＣＤ
     MOVE  HHD-F03                TO   KAN-F04.
*返品日
     MOVE  WK-HHD-F04             TO   KAN-F05.
*返品受領箱数
     MOVE  HHD-F05                TO   KAN-F06.
*返品受領済箱数
     MOVE  HHD-F06                TO   KAN-F07.
*受付担当者ＣＤ
     MOVE  HHD-F07                TO   KAN-F08.
*受付日
     MOVE  WK-HHD-F08             TO   KAN-F09.
*受付時刻
     MOVE  WK-HHD-F09             TO   KAN-F10.
*取込担当者部門ＣＤ
     MOVE  LINK-IN-BUMON          TO   KAN-F96.
*取込担当者ＣＤ
     MOVE  LINK-IN-TANCD          TO   KAN-F97.
*取込日付
     MOVE  LINK-IN-TDATE          TO   KAN-F98.
*取込時刻
     MOVE  LINK-IN-TTIME          TO   KAN-F99.
*
 HENKANL2-OUT-98.
*レコード出力
     WRITE      KAN-REC.
     ADD        1             TO   HENKANL2-OUT-CNT.
*
 HENKANL2-OUT-99.
*次レコードＲＥＡＤ
     PERFORM    RCVHHDXX-READ-SEC.
*
 HENKANL2-OUT-EXIT.
     EXIT.
****************************************************************
*    ホスト返品管理明細ファイル出力
****************************************************************
 HENKAML2-OUT-SEC             SECTION.
     MOVE "HENKAML2-OUT-SEC"      TO   S-NAME.
*
 HENKAML2-OUT-01.
*レコードＲＥＡＤ
     PERFORM    RCVHHMXX-READ-SEC.
     IF         RCVHHMXX-END-FLG =  "END"
                GO                  TO   HENKAML2-OUT-EXIT
     END-IF.
*
*返品日　数値化
     MOVE  HHM-F04(1:4)           TO   WK-HHM-F04-1.
     MOVE  HHM-F04(6:2)           TO   WK-HHM-F04-2.
     MOVE  HHM-F04(9:2)           TO   WK-HHM-F04-3.
*
*存在チェック
     PERFORM  HENKAML2-READ-SEC.
     IF       HENKAML2-INV-FLG =  "   "
              ADD    1            TO   RCVHHMXX-SKIP-CNT
              GO                  TO   HENKAML2-OUT-01
     END-IF.
*
     MOVE  SPACE                  TO   KAM-REC.
     INITIALIZE                        KAM-REC.
*
*倉庫ＣＤ
     MOVE  LINK-IN-SOKCD          TO   KAM-F01.
*返品グループ番号
     MOVE  HHM-F01                TO   KAM-F02.
*取引先ＣＤ
     MOVE  HHM-F02                TO   KAM-F03.
*店舗ＣＤ
     MOVE  HHM-F03                TO   KAM-F04.
*返品日
     MOVE  WK-HHM-F04             TO   KAM-F05.
*返品伝票番号
     MOVE  HHM-F05                TO   KAM-F06.
*取込担当者部門ＣＤ
     MOVE  LINK-IN-BUMON          TO   KAM-F96.
*取込担当者ＣＤ
     MOVE  LINK-IN-TANCD          TO   KAM-F97.
*取込日付
     MOVE  LINK-IN-TDATE          TO   KAM-F98.
*取込時刻
     MOVE  LINK-IN-TTIME          TO   KAM-F99.
*
 HENKAML2-OUT-98.
*レコード出力
     WRITE      KAM-REC.
     ADD        1             TO   HENKAML2-OUT-CNT.
*
 HENKAML2-OUT-EXIT.
     EXIT.
****************************************************************
*    ホスト返品実績ファイル出力
****************************************************************
 HENKJSL2-OUT-SEC             SECTION.
     MOVE "HENKJSL2-OUT-SEC"      TO   S-NAME.
*
 HENKJSL2-OUT-01.
*レコードＲＥＡＤ
     PERFORM    RCVHJHXX-READ-SEC.
     IF         RCVHJHXX-END-FLG =  "END"
                GO                  TO   HENKJSL2-OUT-EXIT
     END-IF.
*
 HENKJSL2-OUT-02.
*レコードＲＥＡＤ
     PERFORM    RCVHJMXX-READ-SEC.
     IF         RCVHJMXX-END-FLG =  "END"
                GO                  TO   HENKJSL2-OUT-EXIT
     END-IF.
*
 HENKJSL2-OUT-03.
     IF         HJH-F05     =       HJM-F01
                GO                  TO   HENKJSL2-OUT-04
     END-IF.
*
     IF         HJH-F05     <       HJM-F01
*               読み飛ばし
                PERFORM     RCVHJHXX-READ-SEC
                IF          RCVHJHXX-END-FLG =  "END"
                            GO      TO   HENKJSL2-OUT-EXIT
                END-IF
                GO                  TO   HENKJSL2-OUT-03
     END-IF.
*
     IF         HJH-F05     >       HJM-F01
*               読み飛ばし
                PERFORM     RCVHJMXX-READ-SEC
                IF          RCVHJMXX-END-FLG =  "END"
                            GO      TO   HENKJSL2-OUT-EXIT
                END-IF
                GO                  TO   HENKJSL2-OUT-03
     END-IF.
*
 HENKJSL2-OUT-04.
*
*返品日　数値化
     MOVE  HJH-F04(1:4)           TO   WK-HJH-F04-1.
     MOVE  HJH-F04(6:2)           TO   WK-HJH-F04-2.
     MOVE  HJH-F04(9:2)           TO   WK-HJH-F04-3.
*返品日検品日　数値化
     MOVE  HJH-F07(1:4)           TO   WK-HJH-F07-1.
     MOVE  HJH-F07(6:2)           TO   WK-HJH-F07-2.
     MOVE  HJH-F07(9:2)           TO   WK-HJH-F07-3.
*検品作業開始時刻　数値化
     MOVE  HJH-F10(1:2)           TO   WK-HJH-F10-1.
     MOVE  HJH-F10(4:2)           TO   WK-HJH-F10-2.
     MOVE  HJH-F10(7:2)           TO   WK-HJH-F10-3.
*検品作業終了時刻　数値化
     MOVE  HJH-F11(1:2)           TO   WK-HJH-F11-1.
     MOVE  HJH-F11(4:2)           TO   WK-HJH-F11-2.
     MOVE  HJH-F11(7:2)           TO   WK-HJH-F11-3.
*
*存在チェック
     PERFORM  HENKJSL2-READ-SEC.
     IF       HENKJSL2-INV-FLG =  "   "
              ADD    1            TO   RCVHJMXX-SKIP-CNT
              GO                  TO   HENKJSL2-OUT-02
     END-IF.
*
*レコード編集
     MOVE  SPACE                  TO   KJS-REC.
     INITIALIZE                        KJS-REC.
*
*倉庫ＣＤ
     MOVE  LINK-IN-SOKCD          TO   KJS-F01.
*返品グループ番号
     MOVE  HJH-F01                TO   KJS-F02.
*取引先ＣＤ
     MOVE  HJH-F02                TO   KJS-F03.
*店舗ＣＤ
     MOVE  HJH-F03                TO   KJS-F04.
*返品日
     MOVE  WK-HJH-F04             TO   KJS-F05.
*返品伝票番号
     MOVE  HJH-F05                TO   KJS-F06.
*返品検品数（合計）
     MOVE  HJH-F06                TO   KJS-F07.
*返品検品日
     MOVE  WK-HJH-F07             TO   KJS-F08.
*HHT端末番号
     MOVE  HJH-F08                TO   KJS-F09.
*検品担当者コード
     MOVE  HJH-F09                TO   KJS-F10.
*検品作業開始時刻
     MOVE  WK-HJH-F10             TO   KJS-F11.
*検品作業終了時刻
     MOVE  WK-HJH-F11             TO   KJS-F12.
*ＪＡＮコード
     MOVE  HJM-F02                TO   KJS-F13.
*返品検品数
     MOVE  HJM-F03                TO   KJS-F14.
*サカタ商品ＣＤ
     MOVE  HJM-F04                TO   KJS-F15.
*サカタ品単１
     MOVE  HJM-F05                TO   KJS-F16.
*サカタ品単２
     MOVE  HJM-F06                TO   KJS-F17.
*サカタ品単３
     MOVE  HJM-F07                TO   KJS-F18.
*ホスト側担当者コード
     MOVE  HJH-F09                TO   TAN-F01.
     PERFORM  TANHENL1-READ-SEC.
     IF  TANHENL1-INV-FLG  =  "INV"
         MOVE "01"                TO   KJS-F95
     ELSE
         MOVE  TAN-F03            TO   KJS-F95
     END-IF.
*取込担当者部門ＣＤ
     MOVE  LINK-IN-BUMON          TO   KJS-F96.
*取込担当者ＣＤ
     MOVE  LINK-IN-TANCD          TO   KJS-F97.
*取込日付
     MOVE  LINK-IN-TDATE          TO   KJS-F98.
*取込時刻
     MOVE  LINK-IN-TTIME          TO   KJS-F99.
*
 HENKJSL2-OUT-98.
*レコード出力
     WRITE      KJS-REC.
     ADD        1             TO   HENKJSL2-OUT-CNT.
*
     GO                       TO   HENKJSL2-OUT-02.
*
 HENKJSL2-OUT-EXIT.
     EXIT.
****************************************************************
*    終了
****************************************************************
 END-SEC                   SECTION.
*
     MOVE     "END-SEC"     TO    S-NAME.
*
     CLOSE    HENKANF   HENKAMF   HENKJSF
              RCVHHDXX  RCVHHMXX  RCVHJHXX  RCVHJMXX  TANHENL1.
*
*    COMPUTE RCVHHDXX-HIT-CNT
*            = RCVHHDXX-IN-CNT - RCVHHDXX-SKIP-CNT.
*    COMPUTE RCVHHMXX-HIT-CNT
*            = RCVHHMXX-IN-CNT - RCVHHMXX-SKIP-CNT.
*    COMPUTE RCVHJMXX-HIT-CNT
*            = RCVHJMXX-IN-CNT - RCVHJMXX-SKIP-CNT.
*
     DISPLAY   "                     "               UPON CONS.
     DISPLAY NC"返品管理Ｆ　　　　読込"  " = "
             RCVHHDXX-IN-CNT             NC"件"      UPON CONS.
*    DISPLAY NC"　　　　　　　　　対象"  " = "
*            RCVHHDXX-HIT-CNT            NC"件"      UPON CONS.
     DISPLAY NC"ホスト返品管理Ｆ　出力"  " = "
             HENKANL2-OUT-CNT            NC"件"      UPON CONS.
*
     DISPLAY   "                     "               UPON CONS.
     DISPLAY NC"返品管理明細Ｆ　　読込"  " = "
             RCVHHMXX-IN-CNT             NC"件"      UPON CONS.
*    DISPLAY NC"　　　　　　　　　対象"  " = "
*            RCVHHMXX-HIT-CNT            NC"件"      UPON CONS.
     DISPLAY NC"ホスト返品明細Ｆ　出力"  " = "
             HENKAML2-OUT-CNT            NC"件"      UPON CONS.
*
     DISPLAY   "                     "               UPON CONS.
     DISPLAY NC"返品実績ヘッダＦ　読込"  " = "
             RCVHJHXX-IN-CNT             NC"件"      UPON CONS.
     DISPLAY NC"返品実績明細Ｆ　　読込"  " = "
             RCVHJMXX-IN-CNT             NC"件"      UPON CONS.
*    DISPLAY NC"　　　　　　　　　対象"  " = "
*            RCVHJMXX-HIT-CNT            NC"件"      UPON CONS.
     DISPLAY NC"ホスト返品実績Ｆ　出力"  " = "
             HENKJSL2-OUT-CNT            NC"件"      UPON CONS.
*
     DISPLAY   MSG-END      UPON  CONS.
*
 END-EXIT.
     EXIT.
****************************************************************
*    返品管理ファイル読込
****************************************************************
 RCVHHDXX-READ-SEC            SECTION.
     MOVE "RCVHHDXX-READ-SEC"     TO   S-NAME.
*
     READ   RCVHHDXX
            AT  END   MOVE "END"  TO   RCVHHDXX-END-FLG
                      GO          TO   RCVHHDXX-READ-EXIT
     END-READ.
*
     ADD    1                     TO   RCVHHDXX-IN-CNT.
*
 RCVHHDXX-READ-EXIT.
     EXIT.
****************************************************************
*    返品管理明細ファイル読込
****************************************************************
 RCVHHMXX-READ-SEC            SECTION.
     MOVE "RCVHHMXX-READ-SEC"     TO   S-NAME.
*
     READ   RCVHHMXX
            AT  END   MOVE "END"  TO   RCVHHMXX-END-FLG
                      GO          TO   RCVHHMXX-READ-EXIT
     END-READ.
*
     ADD    1                     TO   RCVHHMXX-IN-CNT.
*
 RCVHHMXX-READ-EXIT.
     EXIT.
****************************************************************
*    返品実績ヘッダファイル読込
****************************************************************
 RCVHJHXX-READ-SEC            SECTION.
     MOVE "RCVHJHXX-READ-SEC"     TO   S-NAME.
*
     READ   RCVHJHXX
            AT  END   MOVE "END"  TO   RCVHJHXX-END-FLG
                      GO          TO   RCVHJHXX-READ-EXIT
     END-READ.
*
     ADD    1                     TO   RCVHJHXX-IN-CNT.
*
 RCVHJHXX-READ-EXIT.
     EXIT.
****************************************************************
*    返品実績明細ファイル読込
****************************************************************
 RCVHJMXX-READ-SEC            SECTION.
     MOVE "RCVHJMXX-READ-SEC"     TO   S-NAME.
*
     READ   RCVHJMXX
            AT  END   MOVE "END"  TO   RCVHJMXX-END-FLG
                      GO          TO   RCVHJMXX-READ-EXIT
     END-READ.
*
     ADD    1                     TO   RCVHJMXX-IN-CNT.
*
 RCVHJMXX-READ-EXIT.
     EXIT.
***************************************************************
*    ホスト返品管理ファイル　検索
***************************************************************
 HENKANL2-READ-SEC      SECTION.
*
     MOVE "HENKANL2-READ-SEC"     TO  S-NAME.
*
     MOVE    LINK-IN-SOKCD        TO  KAN-F01.
     MOVE    LINK-IN-TDATE        TO  KAN-F98.
     MOVE    LINK-IN-TTIME        TO  KAN-F99.
     MOVE    HHD-F01              TO  KAN-F02.
     MOVE    HHD-F02              TO  KAN-F03.
     MOVE    HHD-F03              TO  KAN-F04.
     MOVE    WK-HHD-F04           TO  KAN-F05.
     READ    HENKANF
       INVALID      KEY
          MOVE     "INV"          TO  HENKANL2-INV-FLG
       NOT INVALID  KEY
          MOVE     "   "          TO  HENKANL2-INV-FLG
     END-READ.
*
 HENKANL2-READ-EXIT.
     EXIT.
***************************************************************
*    ホスト返品管理明細ファイル　検索
***************************************************************
 HENKAML2-READ-SEC      SECTION.
*
     MOVE "HENKAML2-READ-SEC"     TO   S-NAME.
*
     MOVE    LINK-IN-SOKCD        TO   KAM-F01.
     MOVE    LINK-IN-TDATE        TO   KAM-F98.
     MOVE    LINK-IN-TTIME        TO   KAM-F99.
     MOVE    HHM-F01              TO   KAM-F02.
     MOVE    HHM-F02              TO   KAM-F03.
     MOVE    HHM-F03              TO   KAM-F04.
     MOVE    WK-HHM-F04           TO   KAM-F05.
     MOVE    HHM-F05              TO   KAM-F06.
     READ    HENKAMF
       INVALID      KEY
          MOVE     "INV"          TO   HENKAML2-INV-FLG
       NOT INVALID  KEY
          MOVE     "   "          TO   HENKAML2-INV-FLG
     END-READ.
*
 HENKAML2-READ-EXIT.
     EXIT.
***************************************************************
*    ホスト返品実績ファイル　検索
***************************************************************
 HENKJSL2-READ-SEC      SECTION.
*
     MOVE "HENKJSL2-READ-SEC"     TO   S-NAME.
*
     MOVE    LINK-IN-SOKCD        TO   KJS-F01.
     MOVE    LINK-IN-TDATE        TO   KJS-F98.
     MOVE    LINK-IN-TTIME        TO   KJS-F99.
     MOVE    HJH-F01              TO   KJS-F02.
     MOVE    HJH-F02              TO   KJS-F03.
     MOVE    HJH-F03              TO   KJS-F04.
     MOVE    WK-HJH-F04           TO   KJS-F05.
     MOVE    HJH-F05              TO   KJS-F06.
     MOVE    WK-HJH-F10           TO   KJS-F11.
     MOVE    WK-HJH-F11           TO   KJS-F12.
     MOVE    HJM-F02              TO   KJS-F13.
     READ    HENKJSF
       INVALID      KEY
          MOVE     "INV"     TO   HENKJSL2-INV-FLG
       NOT INVALID  KEY
          MOVE     "   "     TO   HENKJSL2-INV-FLG
     END-READ.
*
 HENKJSL2-READ-EXIT.
     EXIT.
***************************************************************
*    担当者変換マスタ　検索
***************************************************************
 TANHENL1-READ-SEC      SECTION.
*
     MOVE "TANHENL1-READ-SEC"     TO  S-NAME.
*
     READ    TANHENL1
       INVALID      KEY
          MOVE     "INV"          TO  TANHENL1-INV-FLG
       NOT INVALID  KEY
          MOVE     "   "          TO  TANHENL1-INV-FLG
     END-READ.
*
 TANHENL1-READ-EXIT.
     EXIT.
