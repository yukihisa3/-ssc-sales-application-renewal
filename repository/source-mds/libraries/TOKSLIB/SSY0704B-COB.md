# SSY0704B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SSY0704B.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　サカタのタネ（株）殿　　　　　　　*
*    業務名　　　　　　　：　出荷管理システム　　　　　　　　　*
*    モジュール名　　　　：　量販店データ伝票_採番（ルート）　*
*    作成日／作成者　　　：　2000/01/27 TAKAHASHI              *
*    再利用ＰＧ　　　　　：                                    *
*    更新日／更新者　　　：　2000/04/20 NAV TAKAHASHI          *
*                            備考マスタからの備考行作成        *
****************************************************************
 IDENTIFICATION              DIVISION.
 PROGRAM-ID.                 SSY0704B.
 ENVIRONMENT                 DIVISION.
 CONFIGURATION               SECTION.
 SOURCE-COMPUTER.
 OBJECT-COMPUTER.
 SPECIAL-NAMES.
     YA            IS        CHR-2
     YB-21         IS        CHR-21
     YB            IS        CHR-15
     CONSOLE       IS        CONS
     STATION       IS        STAT.
****************************************************************
 INPUT-OUTPUT              SECTION.
****************************************************************
 FILE-CONTROL.
*伝票データＦ２
     SELECT      SHTDENWK    ASSIGN    TO        DA-01-S-SHTDENWK
                             FILE      STATUS    DEN2-ST.
*伝票データＦ
*    SELECT      SHTDENF     ASSIGN    TO        DA-01-S-SHTDENF
     SELECT      SHTDENF     ASSIGN    TO        SHTDENF
                             ORGANIZATION        SEQUENTIAL
                             ACCESS    MODE      SEQUENTIAL
                             FILE      STATUS    DEN-ST.
*取引先マスタ
     SELECT      HTOKMS      ASSIGN    TO        DA-01-VI-TOKMS2
                             ORGANIZATION        INDEXED
                             ACCESS    MODE      RANDOM
                             RECORD    KEY       TOK-F01
                             FILE      STATUS    TOK-ST.
*プリント定義ファイル
     SELECT     PRTFILE      ASSIGN    TO        LP-04-PRTF
                             FILE      STATUS    PRT-ST.
*商品在庫マスタ
     SELECT     ZZAIMS       ASSIGN    TO        DA-01-VI-ZZAIMS1
                             ORGANIZATION        INDEXED
                             ACCESS    MODE      RANDOM
                             RECORD    KEY       ZAI-F01
                                                 ZAI-F02
                                                 ZAI-F03
                             FILE      STATUS    ZAI-ST.
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
*量販店備考ワーク
     SELECT     SHTBIKF      ASSIGN    TO        DA-01-VI-SHTBIKL1
                             ORGANIZATION        INDEXED
                             ACCESS    MODE      RANDOM
                             RECORD    KEY       BIK-F01
                                                 BIK-F02
                                                 BIK-F03
                             FILE      STATUS    BIK-ST.
*量販店データ変換リストワーク
     SELECT     DENMEISF     ASSIGN    TO        DA-01-S-DENMEISF
                             FILE      STATUS    DME-ST.
****************************************************************
 DATA                        DIVISION.
****************************************************************
 FILE                        SECTION.
*伝票データＦ
 FD  SHTDENWK
     LABEL       RECORD      IS        STANDARD
     BLOCK       CONTAINS    1         RECORDS.
     COPY        SHTDENWK    OF        XFDLIB
     JOINING     DEN2        AS        PREFIX.
*伝票データＦ
 FD  SHTDENF
     LABEL       RECORD      IS        STANDARD
     BLOCK       CONTAINS    1         RECORDS.
     COPY        SHTDENF     OF        XFDLIB
     JOINING     DEN         AS        PREFIX.
*取引先マスタ
 FD  HTOKMS
     LABEL       RECORD      IS        STANDARD
     BLOCK       CONTAINS     8        RECORDS.
     COPY        HTOKMS      OF        XFDLIB
     JOINING     TOK         AS        PREFIX.
*プリントファイル
 FD  PRTFILE
     LABEL       RECORD    IS        OMITTED.
 01  PRT-REC.
     03  FILLER            PIC X(200).
*商品在庫マスタ
 FD  ZZAIMS.
     COPY     ZZAIMS    OF        XFDLIB
              JOINING   ZAI       PREFIX.
*商品変換テーブル
 FD  HSHOTBL.
     COPY     HSHOTBL   OF        XFDLIB
              JOINING   SHO       PREFIX.
*商品名称マスタ
 FD  HMEIMS.
     COPY     HMEIMS    OF        XFDLIB
              JOINING   MEI       PREFIX.
*量販店備考ワーク
 FD  SHTBIKF.
     COPY     SHTDENWK  OF        XFDLIB
              JOINING   BIK       PREFIX.
*伝票データＦ
 FD  DENMEISF
     LABEL       RECORD      IS        STANDARD
     BLOCK       CONTAINS    1         RECORDS.
     COPY        SHTDENWK    OF        XFDLIB
     JOINING     DME         AS        PREFIX.
****************************************************************
 WORKING-STORAGE           SECTION.
****************************************************************
 01  ST-AREA.
     03  IN-DATA             PIC  X(01)  VALUE  SPACE.
     03  DEN2-ST             PIC  X(02)  VALUE  SPACE.
     03  DEN-ST              PIC  X(02)  VALUE  SPACE.
     03  TOK-ST              PIC  X(02)  VALUE  SPACE.
     03  PRT-ST              PIC  X(02)  VALUE  SPACE.
     03  ZAI-ST              PIC  X(02)  VALUE  SPACE.
     03  SHO-ST              PIC  X(02)  VALUE  SPACE.
     03  MEI-ST              PIC  X(02)  VALUE  SPACE.
     03  BIK-ST              PIC  X(02)  VALUE  SPACE.
     03  DME-ST              PIC  X(02)  VALUE  SPACE.
 01  WK-AREA.
     03  END-FLG             PIC  9(01)  VALUE  ZERO.
     03  I                   PIC  9(01)  VALUE  ZERO.
     03  INV-SW              PIC  9(01)  VALUE  ZERO.
     03  WK-DENCNT           PIC  9(09)  VALUE  ZERO.
     03  IX                  PIC  9(01)  VALUE  ZERO.
     03  WK-TOKCD            PIC  9(08)  VALUE  ZERO.
     03  WK-TENCD            PIC  9(05)  VALUE  ZERO.
     03  WK-DENNO            PIC  9(09)  VALUE  ZERO.
*\\\ 93.06.04 START \\\
     03  WK-SYUKA            PIC  9(08)  VALUE  ZERO.
     03  WK-BASYO            PIC  X(02)  VALUE  SPACE.
*\\\ 93.06.04 END   \\\
     03  WK-TOKNM            PIC  N(10).
     03  L-CNT               PIC  99     VALUE  ZERO.
     03  P-CNT               PIC  99     VALUE  ZERO.
     03  KEP-FLG             PIC  X(01)  VALUE  SPACE.
     03  HSHOTBL-INV-FLG     PIC  X(03)  VALUE  SPACE.
     03  HMEIMS-INV-FLG      PIC  X(03)  VALUE  SPACE.
     03  BIKO-INV-FLG        PIC  X(03)  VALUE  SPACE.
     03  WK-DENBIKO          PIC  9(09)  VALUE  ZERO.
     03  WK-DENPYO           PIC  9(09)  VALUE  ZERO.
     03  WK-TORBIKO          PIC  9(08)  VALUE  ZERO.
*計算領域
 01  WRK-AREA.
     03  WRK-HIK             PIC S9(09)V9(02)  VALUE ZERO.
     03  WRK-ZAI             PIC S9(09)V9(02)  VALUE ZERO.
*
 01  DENNO-S                 PIC  9(09)  VALUE  999999999.
 01  DENNO-L                 PIC  9(09)  VALUE  ZERO.
 01  WK-DENBK                PIC  9(09)  VALUE  ZERO.
*日付取得
 01  SYS-DATE                PIC  9(06)  VALUE  ZERO.
 01  WK-DATE8.
     03  WK-Y                PIC  9(04)  VALUE  ZERO.
     03  WK-M                PIC  9(02)  VALUE  ZERO.
     03  WK-D                PIC  9(02)  VALUE  ZERO.
***  エラーセクション名
 01  SEC-NAME.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-SEC    => ".
     03  S-NAME                   PIC  X(20).
*
*---<< ｻﾌﾞﾙｰﾁﾝ LINK AREA >>-*
 01  LINK-AREA.
     03  LINK-IN.
         05  LI-KBN          PIC  9(01).
         05  LI-KETA         PIC  9(01).
         05  LI-START        PIC  9(09).
         05  LI-END          PIC  9(09).
         05  LI-DENNO        PIC  9(09).
     03  LINK-OUT.
         05  LO-ERR          PIC  9(01).
         05  LO-NEXT         PIC  9(09).
*
 01  FILE-ERR.
     03  DEN2-ERR            PIC  N(10)  VALUE
                   NC"伝票データＦ２異常".
     03  DEN-ERR             PIC  N(10)  VALUE
                   NC"伝票データＦ異常".
     03  TOK-ERR             PIC  N(10)  VALUE
                   NC"取引先マスタ異常".
     03  PRT-ERR             PIC  N(10)  VALUE
                   NC"プリントＦ異常".
     03  ZAI-ERR             PIC  N(10)  VALUE
                   NC"在庫マスタ異常".
     03  SHO-ERR             PIC  N(10)  VALUE
                   NC"商品変換テーブル異常".
     03  MEI-ERR             PIC  N(10)  VALUE
                   NC"商品名称マスタ異常".
     03  BIK-ERR             PIC  N(10)  VALUE
                   NC"量販店備考ワーク異常".
     03  DME-ERR             PIC  N(10)  VALUE
                   NC"量販店変換リスト異常".
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN             PIC X(01).
 01  LINK-IN-YMD6            PIC 9(06).
 01  LINK-IN-YMD8            PIC 9(08).
 01  LINK-OUT-RET            PIC X(01).
 01  LINK-OUT-YMD            PIC 9(08).
*
****************************************************************
*    プリントエリア                                            *
****************************************************************
*--------------------------------------------------------------*
*    ヘッダ                                                    *
*--------------------------------------------------------------*
*
 01  HD1.
     03  FILLER                  PIC  X(35)  VALUE  SPACE.
     03  FILLER                  PIC  N(22)  VALUE
         NC"※※　量販店伝票_採番リスト（ルート）　※※"
                                 CHARACTER  TYPE  IS  CHR-21.
     03  FILLER                  PIC  X(08)  VALUE  SPACE.
     03  HD1-01                  PIC  9(04).
     03  FILLER                  PIC  N(01)  VALUE  NC"年"
                                 CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  HD1-02                  PIC  Z9.
     03  FILLER                  PIC  N(01)  VALUE  NC"月"
                                 CHARACTER  TYPE  IS  CHR-2.
     03  HD1-03                  PIC  Z9.
     03  FILLER                  PIC  N(01)  VALUE  NC"日"
                                 CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(03)  VALUE  SPACE.
     03  HD1-04                  PIC  ZZ9.
     03  FILLER                  PIC  N(01)  VALUE  NC"頁"
                                 CHARACTER  TYPE  IS  CHR-2.
*
 01  HD2.
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  FILLER                  PIC  N(04)  VALUE NC"端末名："
                                 CHARACTER  TYPE  IS  CHR-2.
     03  HD2-01                  PIC  X(08)  VALUE  SPACE.
     03  FILLER                  PIC  X(22)  VALUE  SPACE.
*
 01  HD3.
     03  FILLER                  PIC  X(35)  VALUE  SPACE.
     03  FILLER                  PIC  N(03)  VALUE
                                 NC"取引先"
                                 CHARACTER   TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(35)  VALUE  SPACE.
     03  FILLER                  PIC  N(05)  VALUE
                                 NC"開始伝票_"
                                 CHARACTER   TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(05)  VALUE  SPACE.
     03  FILLER                  PIC  N(05)  VALUE
                                 NC"終了伝票_"
                                 CHARACTER  TYPE  IS  CHR-2.
*
 01  SEN                         CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  N(25)  VALUE
         NC"─────────────────────────".
     03  FILLER                  PIC  N(25)  VALUE
         NC"─────────────────────────".
     03  FILLER                  PIC  N(18)  VALUE
         NC"──────────────────".
*01  SEN1.
*    03  FILLER                  PIC  X(50)  VALUE
*        "--------------------------------------------------".
*    03  FILLER                  PIC  X(50)  VALUE
*        "--------------------------------------------------".
*    03  FILLER                  PIC  X(36)  VALUE
*        "------------------------------------".
 01  DT1                         CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(35)  VALUE  SPACE.
     03  DT1-01                  PIC  9(08).
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  DT1-02                  PIC  N(10).
     03  FILLER                  PIC  X(11)  VALUE  SPACE.
     03  DT1-03                  PIC  9(09).
     03  FILLER                  PIC  X(06)  VALUE  SPACE.
     03  DT1-04                  PIC  9(09).
****************************************************************
 LINKAGE                     SECTION.
****************************************************************
 01  LINK-WKSTNNM                PIC  X(08).
****************************************************************
 PROCEDURE                   DIVISION  USING LINK-WKSTNNM.
****************************************************************
 DECLARATIVES.
 DEN2-ERR                    SECTION.
     USE         AFTER       EXCEPTION PROCEDURE SHTDENWK.
     DISPLAY     DEN2-ERR    UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     DEN2-ST     UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     ACCEPT      IN-DATA     FROM      CONS.
     STOP        RUN.
 DEN-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE SHTDENF.
     DISPLAY     DEN-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     DEN-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     ACCEPT      IN-DATA     FROM      CONS.
     STOP        RUN.
 TOK-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE HTOKMS.
     DISPLAY     TOK-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     TOK-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     ACCEPT      IN-DATA     FROM      CONS.
     STOP        RUN.
 PRT-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE PRTFILE.
     DISPLAY     PRT-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     PRT-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     ACCEPT      IN-DATA     FROM      CONS.
     STOP        RUN.
 ZAI-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE ZZAIMS.
     DISPLAY     ZAI-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     ZAI-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     ACCEPT      IN-DATA     FROM      CONS.
     STOP        RUN.
 SHO-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE HSHOTBL.
     DISPLAY     SHO-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     SHO-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     ACCEPT      IN-DATA     FROM      CONS.
     STOP        RUN.
 MEI-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE HMEIMS.
     DISPLAY     MEI-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     MEI-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     ACCEPT      IN-DATA     FROM      CONS.
     STOP        RUN.
 BIK-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE SHTBIKF.
     DISPLAY     BIK-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     BIK-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     ACCEPT      IN-DATA     FROM      CONS.
     STOP        RUN.
 DME-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE DENMEISF.
     DISPLAY     DME-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     DME-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     ACCEPT      IN-DATA     FROM      CONS.
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
     OPEN        I-O         SHTDENWK  HTOKMS  ZZAIMS
                 INPUT       HSHOTBL   HMEIMS  SHTBIKF
                 EXTEND      SHTDENF
                 OUTPUT      PRTFILE   DENMEISF.
*システム日付・時刻の取得
     ACCEPT   SYS-DATE          FROM   DATE.
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
     MOVE      LINK-OUT-YMD       TO   WK-DATE8.
*
*量販ＲＥＣ初期化
     MOVE        SPACE       TO   DEN-REC.
     INITIALIZE  DEN-REC.
 INIT-010.
*伝票データ２初期読み
     PERFORM  DEN2-RD-SEC.
     IF    END-FLG  =  0
       MOVE     DEN2-F01       TO   WK-TOKCD
       MOVE     DEN2-F02       TO   WK-DENNO
       MOVE     DEN2-F07       TO   WK-TENCD
*\\\ 93.06.04 START \\\
       MOVE     DEN2-F112      TO   WK-SYUKA
       MOVE     DEN2-F08       TO   WK-BASYO
*\\\ 93.06.04 END   \\\
*取引先マスタＲＥＡＤ
       PERFORM  TOK-RD-SEC
       IF   INV-SW  =  1
              GO        TO        INIT-010
       END-IF
     END-IF.
     PERFORM    HEAD-WT-SEC.
 INIT-EXIT.
     EXIT.
****************************************************************
*                 M A I N - S E C
****************************************************************
 MAIN-SEC                    SECTION.
     MOVE     "MAIN-SEC"          TO   S-NAME.
*
     IF  L-CNT    >  60
*         改頁
          MOVE          SPACE     TO        PRT-REC
          WRITE   PRT-REC  AFTER  PAGE
          MOVE    ZERO     TO     L-CNT
          PERFORM  HEAD-WT-SEC
     END-IF.
*
*取引先コードがブレイク
     IF  DEN2-F01 NOT = WK-TOKCD
         PERFORM   BODY-WT-SEC
         PERFORM   TOK-WT-SEC
         MOVE      DEN2-F01       TO   WK-TOKCD
         MOVE      DEN2-F02       TO   WK-DENNO
         MOVE      DEN2-F07       TO   WK-TENCD
         MOVE      DEN2-F112      TO   WK-SYUKA
         MOVE      DEN2-F08       TO   WK-BASYO
         MOVE      999999999      TO   DENNO-S
         MOVE      ZERO           TO   DENNO-L
         PERFORM   BIKOU-ADD-SEC
         PERFORM   TOK-RD-SEC
         IF   (INV-SW  =  1)
           GO             TO        DEN2-READ
         END-IF
     END-IF.
*
*指定伝票_又は店舗コードがブレイク
     IF  DEN2-F02 NOT = WK-DENNO
     OR  DEN2-F07 NOT = WK-TENCD
     OR  DEN2-F112 NOT = WK-SYUKA
     OR  DEN2-F08  NOT = WK-BASYO
         PERFORM   BIKOU-ADD-SEC
         PERFORM   DEN-GET-SEC
         MOVE      0              TO   IX
         MOVE      DEN2-F02       TO   WK-DENNO
         MOVE      DEN2-F07       TO   WK-TENCD
         MOVE      DEN2-F112      TO   WK-SYUKA
         MOVE      DEN2-F08       TO   WK-BASYO
     END-IF.
*行_は６まで
     ADD   1       TO     IX.
     IF    IX      >   6
         PERFORM   BIKOU-ADD-SEC
         PERFORM   DEN-GET-SEC
         MOVE      1              TO   IX
     END-IF.
*
*伝票データ作成
     MOVE     DEN2-REC       TO        DEN-REC.
 MAIN061.
     MOVE     WK-DENCNT      TO        DEN-F02.
     MOVE     WK-DENCNT      TO        DEN-F23.
     MOVE     ZERO           TO        DEN-F113.
     MOVE     IX             TO        DEN-F03.
*
       IF       DEN-F02        <         DENNO-S
                MOVE    DEN-F02   TO     DENNO-S
       END-IF.
       IF       DEN-F02        >         DENNO-L
                MOVE    DEN-F02   TO     DENNO-L
       END-IF.
*
 MAIN062.
*商品変換ＴＢＬチェック*
     PERFORM     HSHOTBL-READ-SEC.
     IF  HSHOTBL-INV-FLG  =  SPACE
*        *商品名称マスタチェック*
         MOVE    SHO-F031  TO        MEI-F011
         MOVE    SHO-F032  TO        MEI-F012
         PERFORM  HMEIMS-READ-SEC
     END-IF.
*在庫引当
     IF  HSHOTBL-INV-FLG  =  SPACE
         PERFORM  ZAIKO-SEC
         IF  KEP-FLG  =  SPACE
             MOVE     1       TO     DEN-F27D
         END-IF
         MOVE         1       TO     DEN-F262
     END-IF.
*伝票データ追加
     WRITE    DEN-REC.
 MAIN063.
*伝票番号を退避
     MOVE     DEN2-F02        TO     WK-DENBIKO.
     MOVE     DEN-F02         TO     WK-DENPYO.
     MOVE     DEN-F01         TO     WK-TORBIKO.
     PERFORM  DEN2-WT-SEC.
*
*伝票データ２リード
 DEN2-READ.
     PERFORM  DEN2-RD-SEC.
     IF    END-FLG  =  9
         PERFORM   BODY-WT-SEC
         PERFORM   BIKOU-ADD-SEC
         PERFORM   TOK-WT-SEC
     END-IF.
 MAIN-EXIT.
     EXIT.
****************************************************************
*               伝票データ２ＲＥＡＤ
****************************************************************
 DEN2-RD-SEC                 SECTION.
     MOVE     "DEN2-RD-SEC"       TO   S-NAME.
*
     READ     SHTDENWK  AT   END
              MOVE      9         TO   END-FLG
              GO        TO        DEN2-RD-EXIT.
 DEN2-RD-EXIT.
     EXIT.
****************************************************************
*              伝票データ２ ＲＥＷＲＩＴＥ
****************************************************************
 DEN2-WT-SEC                 SECTION.
     MOVE     "DEN2-WT-SEC"       TO   S-NAME.
*
     MOVE     WK-DENCNT           TO   DEN2-F02.
     MOVE     IX                  TO   DEN2-F03.
*
     MOVE     DEN2-REC            TO   DME-REC.
     WRITE    DME-REC.
*
 DEN2-WT-EXIT.
     EXIT.
****************************************************************
*               取 引 先 マ ス タ Ｒ Ｅ Ａ Ｄ
****************************************************************
 TOK-RD-SEC                  SECTION.
     MOVE     "TOK-RD-SEC"        TO   S-NAME.
*
     MOVE     ZERO           TO   INV-SW
     MOVE     WK-TOKCD       TO   TOK-F01.
     READ     HTOKMS
       INVALID
              MOVE      1         TO   INV-SW
       NOT INVALID
              MOVE      TOK-F54   TO   WK-DENCNT
              MOVE      TOK-F03   TO   WK-TOKNM
              PERFORM   DEN-GET-SEC
              MOVE      ZERO      TO   IX
     END-READ.
 TOK-RD-EXIT.
     EXIT.
****************************************************************
*              取引先マスタ ＲＥＷＲＩＴＥ
****************************************************************
 TOK-WT-SEC                  SECTION.
     MOVE     "TOK-WT-SEC"        TO   S-NAME.
*
     MOVE     WK-DENCNT      TO   TOK-F54.
     REWRITE  TOK-REC.
 TOK-WT-EXIT.
     EXIT.
******************************************************************
*                  伝票番号取得
******************************************************************
 DEN-GET-SEC                 SECTION.
     MOVE     "DEN-GET-SEC"       TO   S-NAME.
*
     INITIALIZE              LINK-AREA.
     MOVE     TOK-F93        TO   LI-KBN.
     MOVE     TOK-F92        TO   LI-KETA.
     MOVE     TOK-F57        TO   LI-START.
     MOVE     TOK-F58        TO   LI-END.
     MOVE     WK-DENCNT      TO   LI-DENNO.
 DEN-010.
     CALL     "OSKTCDCK"     USING     LINK-AREA.
 DEN-020.
     IF       LO-ERR  =  0
              MOVE      LO-NEXT   TO   WK-DENCNT
              MOVE      LO-NEXT   TO   TOK-F54
     ELSE
              DISPLAY   NC"伝票_採番エラー"  UPON CONS
              DISPLAY   WK-DENCNT             UPON CONS
              DISPLAY   TOK-ST      UPON      CONS
**************DISPLAY   TOK-ST1     UPON      CONS
**************MOVE      255         TO        PROGRAM-STATUS
              ACCEPT    IN-DATA     FROM      CONS
              STOP  RUN
     END-IF.
 DEN-GET-EXIT.
     EXIT.
****************************************************************
*             ヘッダ部出力処理                                 *
****************************************************************
 HEAD-WT-SEC                  SECTION.
     MOVE     "HEAD-WT-SEC"       TO   S-NAME.
*ページカウント
     ADD      1         TO        P-CNT.
*項目設定
***  日付
     MOVE     WK-Y                TO        HD1-01.
     MOVE     WK-M                TO        HD1-02.
     MOVE     WK-D                TO        HD1-03.
***  ページ_
     MOVE     P-CNT               TO        HD1-04.
***  ワークステーション名
     MOVE     LINK-WKSTNNM        TO        HD2-01.
*
*ヘッダ部出力
     WRITE    PRT-REC      FROM   HD1       AFTER  3.
     WRITE    PRT-REC      FROM   HD2       AFTER  1.
     WRITE    PRT-REC      FROM   HD3       AFTER  1.
     WRITE    PRT-REC      FROM   SEN       AFTER  1.
*
     ADD      6            TO        L-CNT.
 HEAD-WT-EXIT.
     EXIT.
****************************************************************
*             明細部出力処理　                                 *
****************************************************************
 BODY-WT-SEC                  SECTION.
     MOVE     "BODY-WT-SEC"       TO   S-NAME.
*
***  取引先コード
     MOVE     WK-TOKCD            TO   DT1-01.
***  取引先名
     MOVE     WK-TOKNM            TO   DT1-02.
***  開始伝票_
     MOVE     DENNO-S             TO   DT1-03.
***  終了伝票_
     MOVE     DENNO-L             TO   DT1-04.
*明細部出力
     WRITE    PRT-REC      FROM   DT1       AFTER  1.
*
     ADD      1            TO        L-CNT.
 BODY-WT-EXIT.
     EXIT.
****************************************************************
*      2.2     在庫引当                                        *
****************************************************************
 ZAIKO-SEC              SECTION.
*
     MOVE     "ZAIKO-SEC"         TO   S-NAME.
*商品在庫マスタ存在チェック
     MOVE    DEN-F08         TO   ZAI-F01.
     MOVE    DEN-F1411       TO   ZAI-F021.
     MOVE    DEN-F1412       TO   ZAI-F022.
     MOVE    SHO-F08         TO   ZAI-F03.
     READ    ZZAIMS
             INVALID
             PERFORM   ZAIKO-UPDATE1-SEC
             NOT  INVALID
             PERFORM   ZAIKO-UPDATE2-SEC
     END-READ.
*
 ZAIKO-EXIT.
     EXIT.
****************************************************************
*      2.2     在庫引当                                        *
****************************************************************
 ZAIKO-UPDATE1-SEC      SECTION.
*
     MOVE     "ZAIKO-UPDATE1-SEC" TO   S-NAME.
*商品在庫Ｍが未存在の為、在庫マスタ作成
      MOVE      "1"           TO   KEP-FLG.
*商品在庫マスタ初期化
      MOVE      SPACE         TO   ZAI-REC.
      INITIALIZE                   ZAI-REC.
*商品在庫マスタ項目セット
      MOVE      DEN-F08       TO   ZAI-F01.
      MOVE      DEN-F1411     TO   ZAI-F021.
      MOVE      DEN-F1412     TO   ZAI-F022.
      MOVE      SHO-F08       TO   ZAI-F03.
*未出庫数＝未出庫数＋数量
      COMPUTE   ZAI-F13       =    ZAI-F13  +  DEN-F15.
*商品名称マスタ読込み
      PERFORM   HMEIMS-READ-SEC.
*商品名称マスタ存在チェック
      IF  HMEIMS-INV-FLG  =  SPACE
          MOVE  MEI-F031      TO   ZAI-F04
          WRITE ZAI-REC
      END-IF.
*
 ZAIKO-UPDATE1-EXIT.
      EXIT.
****************************************************************
*      2.2     在庫引当                                        *
****************************************************************
 ZAIKO-UPDATE2-SEC      SECTION.
*
     MOVE     "ZAIKO-UPDATE2-SEC" TO   S-NAME.
*引当後在庫数チェック
*    現在庫数－引当済数＝引当可能在庫数
     COMPUTE   WRK-ZAI   =   ZAI-F06  -  ZAI-F14.
*    引当可能在庫数－発注数量＝引当後在庫数
     COMPUTE   WRK-HIK   =   WRK-ZAI  -  DEN-F15.
     IF  WRK-HIK  <  0
         MOVE      "1"      TO  KEP-FLG
*        未出庫数に数量加算
         COMPUTE  ZAI-F13   =   ZAI-F13  +  DEN-F15
*        商品在庫マスタ更新
         REWRITE  ZAI-REC
     ELSE
         MOVE     SPACE     TO  KEP-FLG
*        引当済数に数量加算
         COMPUTE  ZAI-F14   =   ZAI-F14  +  DEN-F15
*        未出庫数に数量加算
         COMPUTE  ZAI-F13   =   ZAI-F13  +  DEN-F15
*        商品在庫マスタ更新
         REWRITE  ZAI-REC
     END-IF.
*
 ZAIKO-UPDATE2-EXIT.
****************************************************************
*                商品変換テーブル読込み                        *
****************************************************************
 HSHOTBL-READ-SEC          SECTION.
*
     MOVE      "HSHOTBL-READ-SEC" TO    S-NAME.
*
     MOVE      DEN-F01     TO     SHO-F01.
     MOVE      DEN-F25     TO     SHO-F02.
     READ      HSHOTBL
               INVALID
               MOVE      "INV"    TO    HSHOTBL-INV-FLG
               NOT  INVALID
               MOVE      SPACE    TO    HSHOTBL-INV-FLG
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
*
     MOVE      SHO-F031    TO     MEI-F011.
     MOVE      SHO-F032    TO     MEI-F012.
     READ      HMEIMS
               INVALID
               MOVE      "INV"    TO    HMEIMS-INV-FLG
               NOT  INVALID
               MOVE      SPACE    TO    HMEIMS-INV-FLG
     END-READ.
*
 HMEIMS-READ-EXIT.
     EXIT.
****************************************************************
*    備考明細チェック
****************************************************************
 BIKOU-ADD-SEC             SECTION.
*
     MOVE       WK-TORBIKO     TO        BIK-F01.
     MOVE       WK-DENBIKO     TO        BIK-F02.
     MOVE       80             TO        BIK-F03.
     READ       SHTBIKF
                INVALID
                MOVE    "INV"  TO        BIKO-INV-FLG
                NOT INVALID
                MOVE    SPACE  TO        BIKO-INV-FLG
     END-READ.
*
     IF  BIKO-INV-FLG  =  SPACE
                MOVE  SPACE    TO        DEN-REC DME-REC
                INITIALIZE               DEN-REC DME-REC
                MOVE  BIK-REC   TO       DEN-REC DME-REC
                MOVE  WK-DENPYO TO       DEN-F02 DME-F02
                MOVE  WK-DENPYO TO       DEN-F23 DME-F23
                MOVE  80        TO       DEN-F03 DME-F03
                WRITE    DME-REC
                WRITE    DEN-REC
     END-IF.
*
 BIKOU-ADD-EXIT.
     EXIT.
****************************************************************
*    終了
****************************************************************
 END-SEC                   SECTION.
*
*     DISPLAY  "＊＊量販店　データ変換＊＊＊" UPON CONS.
*     DISPLAY  "＊＊開始伝票_　＝　" DENNO-S UPON CONS.
*     DISPLAY  "＊＊終了伝票_　＝　" DENNO-L UPON CONS.
*
     CLOSE SHTDENWK SHTDENF PRTFILE HTOKMS HSHOTBL HMEIMS ZZAIMS
           SHTBIKF  DENMEISF.
 END-EXIT.
     EXIT.

```
