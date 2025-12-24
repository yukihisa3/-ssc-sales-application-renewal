# SSY3915V

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SSY3915V.COB`

## ソースコード

```cobol
***********************************************************
*    顧客名　　　　　：（株）サカタのタネ殿　　　　       *
*    サブシステム　　：ナフコＥＤＩ受信システム　         *
*    業務名　　　　　：ナフコＥＤＩ受信                   *
*    モジュール名　　：ナフコ送り状／納品明細書データ作成 *
*    作成日／作成者　：2019/12/05    ASS.TAKAHASHI        *
*    処理概要　　　　：                                   *
*      箱数ファイル、数量訂正ファイルよりナフコ送り状TOと *
*      納品明細書データを作成する。                       *
*    更新日／更新者　：2020/05/26    NAV TAKAHASHI        *
*    更新内容　　　　：明細全て０は出力無しに変更　　     *
*    更新日／更新者　：2020/06/01    NAV TAKAHASHI        *
*    更新内容　　　　：０判定を出荷梱包数からトレー数変更 *
***********************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SSY3915V.
 AUTHOR.               ASS.
 DATE-WRITTEN.         2019/12/05.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.

* 箱数ファイル
     SELECT  NFHAKOF
       ASSIGN         TO  NFHAKOL3
       ORGANIZATION   IS  INDEXED
       ACCESS MODE    IS  SEQUENTIAL
       RECORD KEY     IS  HK3-F01   *> 管理番号
                          HK3-F06   *> 店舗ＣＤ
                          HK3-F07   *> 納品場所
                          HK3-F08   *> 店着日
                          HK3-F05   *> 作場ＣＤ
       FILE STATUS    IS  HK3-ST.

* 数量訂正ファイル
     SELECT  NFSUTEF
       ASSIGN         TO  NFSUTEL3
       ORGANIZATION   IS  INDEXED
       ACCESS MODE    IS  SEQUENTIAL
       RECORD KEY     IS  ST3-F01   *> 管理番号
                          ST3-F06   *> 店舗ＣＤ
                          ST3-F07   *> 納品場所
                          ST3-F08   *> 店着日
                          ST3-F05   *> 作場ＣＤ
                          ST3-F09   *> 伝票番号
       FILE STATUS    IS  ST3-ST.

* 基本情報ファイル
     SELECT  NFJOHOF
       ASSIGN         TO  NFJOHOL1
       ORGANIZATION   IS  INDEXED
       ACCESS MODE    IS  RANDOM
       RECORD KEY     IS  KH1-F01   *> 管理番号
                          KH1-F05   *> 作場ＣＤ
                          KH1-F06   *> 店舗ＣＤ
                          KH1-F07   *> 伝票番号
                          KH1-F08   *> 行番号
                          KH1-F09   *> 納品日
       FILE STATUS    IS  KH1-ST.

* 作場マスタ
     SELECT  SAKUBAF
       ASSIGN         TO  SAKUBAL1
       ORGANIZATION   IS  INDEXED
       ACCESS MODE    IS  RANDOM
       RECORD KEY     IS  SKB-F01   *> 作場ＣＤ
       FILE STATUS    IS  SKB-ST.

* ナフコ商品マスタ
     SELECT  NFSHOMS
       ASSIGN         TO  NFSHOMS1
       ORGANIZATION   IS  INDEXED
       ACCESS MODE    IS  RANDOM
       RECORD KEY     IS  SYO-F01   *> ナフコ商品ＣＤ
       FILE STATUS    IS  SYO-ST.

* ナフコ店舗マスタ
     SELECT  NFTENMS
       ASSIGN         TO  NFTENMS1
       ORGANIZATION   IS  INDEXED
       ACCESS MODE    IS  RANDOM
       RECORD KEY     IS  TEN-F01   *> 取引先ＣＤ
                          TEN-F02   *> 店舗ＣＤ
       FILE STATUS    IS  TEN-ST.

* ナフコ発注累積データ
     SELECT  NFHACPF
       ASSIGN         TO  NFHACL2
       ORGANIZATION   IS  INDEXED
       ACCESS MODE    IS  RANDOM
       RECORD KEY     IS  HC2-A06   *> 法人ＣＤ
                          HC2-A83   *> 納品先店舗ＣＤ
                          HC2-A88   *> 納品場所ＣＤ
                          HC2-A26   *> 納品予定日
                          HC2-A23   *> 伝票番号
                          HC2-A24   *> 行番号
       FILE STATUS    IS  HC2-ST.

* 取引先マスタ
     SELECT  HTOKMS
       ASSIGN         TO  TOKMS2
       ORGANIZATION   IS  INDEXED
       ACCESS MODE    IS  RANDOM
       RECORD KEY     IS  TOK-F01   *> 取引先ＣＤ
       FILE STATUS    IS  TOK-ST.

* ナフコ送り状データ
     SELECT  NFLISTF1
       ASSIGN         TO  DA-01-S-NFLISTF1
       FILE STATUS    IS  SF1-ST.

* ナフコ納品明細書データ
     SELECT  NFLISTF2
       ASSIGN         TO  DA-01-S-NFLISTF2
       FILE STATUS    IS  SF2-ST.

*
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = 箱数ファイル                                       *
****************************************************************
 FD  NFHAKOF
                       LABEL     RECORD    IS   STANDARD.
**     COPY     NFHAKOF
**              DISJOINING XXX JOINING HK3 AS PREFIX.
                       COPY      NFHAKOF   OF   XFDLIB
                       JOINING   HK3       AS   PREFIX.
****************************************************************
*    FILE = 数量訂正ファイル                                   *
****************************************************************
 FD  NFSUTEF
                       LABEL     RECORD    IS   STANDARD.
**     COPY     NFSUTEF
**              DISJOINING XXX JOINING ST3 AS PREFIX.
                       COPY      NFSUTEF   OF   XFDLIB
                       JOINING   ST3       AS   PREFIX.
****************************************************************
*    FILE = 基本情報ファイル                                   *
****************************************************************
 FD  NFJOHOF
                       LABEL     RECORD    IS   STANDARD.
**     COPY     NFJOHOF
**              DISJOINING XXX JOINING KH1 AS PREFIX.
                       COPY      NFJOHOF   OF   XFDLIB
                       JOINING   KH1       AS   PREFIX.
****************************************************************
*    FILE = 作場マスタ                                         *
****************************************************************
 FD  SAKUBAF
                       LABEL     RECORD    IS   STANDARD.
**     COPY     SAKUBAF
**              DISJOINING XXX JOINING SKB AS PREFIX.
                       COPY      SAKUBAF   OF   XFDLIB
                       JOINING   SKB       AS   PREFIX.
****************************************************************
*    FILE = ナフコ商品マスタ                                   *
****************************************************************
 FD  NFSHOMS
                       LABEL     RECORD    IS   STANDARD.
**     COPY     NFSHOMS
**              DISJOINING XXX JOINING SYO AS PREFIX.
                       COPY      NFSHOMS   OF   XFDLIB
                       JOINING   SYO       AS   PREFIX.
****************************************************************
*    FILE = ナフコ店舗マスタ                                   *
****************************************************************
 FD  NFTENMS
                       LABEL     RECORD    IS   STANDARD.
**     COPY     NFTENMS
**              DISJOINING XXX JOINING TEN AS PREFIX.
                       COPY      NFTENMS   OF   XFDLIB
                       JOINING   TEN       AS   PREFIX.
****************************************************************
*    FILE = ナフコ発注累積データ                               *
****************************************************************
 FD  NFHACPF
                       LABEL     RECORD    IS   STANDARD.
**     COPY     NFHACPF
**              DISJOINING XXX JOINING HC2 AS PREFIX.
                       COPY      NFHACPF   OF   XFDLIB
                       JOINING   HC2       AS   PREFIX.
****************************************************************
*    FILE = 取引先マスタ                                       *
****************************************************************
 FD  HTOKMS
                       LABEL     RECORD    IS   STANDARD.
**     COPY     HTOKMS
**              DISJOINING XXX JOINING TOK AS PREFIX.
                       COPY      HTOKMS    OF   XFDLIB
                       JOINING   TOK       AS   PREFIX.
****************************************************************
*    FILE = ナフコ送り状データ                       *
****************************************************************
 FD  NFLISTF1          BLOCK     CONTAINS  1    RECORDS
                       LABEL     RECORD    IS   STANDARD.
**     COPY     NFLISTF1
**              DISJOINING XXX JOINING SF1 AS PREFIX.
                       COPY      NFLISTF1  OF   XFDLIB
                       JOINING   SF1       AS   PREFIX.
****************************************************************
*    FILE = ナフコ納品明細書データ                         *
****************************************************************
 FD  NFLISTF2          BLOCK     CONTAINS  1    RECORDS
                       LABEL     RECORD    IS   STANDARD.
**     COPY     NFLISTF2
**              DISJOINING XXX JOINING SF2 AS PREFIX.
                       COPY      NFLISTF2  OF   XFDLIB
                       JOINING   SF2       AS   PREFIX.

****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  HK3-ST             PIC  X(02).
     03  ST3-ST             PIC  X(02).
     03  KH1-ST             PIC  X(02).
     03  SKB-ST             PIC  X(02).
     03  SYO-ST             PIC  X(02).
     03  TEN-ST             PIC  X(02).
     03  HC2-ST             PIC  X(02).
     03  TOK-ST             PIC  X(02).
     03  SF1-ST             PIC  X(02).
     03  SF2-ST             PIC  X(02).
***  エラーセクション名
 01  SEC-NAME.
     03  FILLER             PIC  X(05)  VALUE " *** ".
     03  S-NAME             PIC  X(30).
***  エラーファイル
 01  FILE-ERR.
     03  HK3-ERR           PIC N(20) VALUE
         NC"箱数ファイル３・エラー".
     03  ST3-ERR           PIC N(20) VALUE
         NC"数量訂正ファイル３・エラー".
     03  KH1-ERR           PIC  N(20)  VALUE
         NC"基本情報ファイル１・エラー".
     03  SKB-ERR           PIC N(20) VALUE
         NC"作場マスタ１・エラー".
     03  SYO-ERR           PIC N(20) VALUE
         NC"ナフコ商品マスタ１・エラー".
     03  TEN-ERR           PIC N(20) VALUE
         NC"ナフコ店舗マスタ１・エラー".
     03  HC2-ERR           PIC N(20) VALUE
         NC"ナフコ発注累積データ２・エラー".
     03  TOK-ERR           PIC N(20) VALUE
         NC"取引先マスタ２・エラー".
     03  SF1-ERR           PIC N(20) VALUE
         NC"ナフコ送り状データエラー".
     03  SF2-ERR           PIC N(20) VALUE
         NC"ナフコ納品明細書データエラー".

*読込フラグ領域
 01  FLG-AREA.
     03  FG-END             PIC  X(03)  VALUE SPACE.
     03  FG-NFHAKOF-END     PIC  X(03)  VALUE SPACE.
     03  FG-NFSUTEF-END     PIC  X(03)  VALUE SPACE.
     03  FG-NFJOHOF-INV     PIC  9(01)  VALUE ZERO.
     03  FG-SAKUBAF-INV     PIC  9(01)  VALUE ZERO.
     03  FG-NFSHOMS-INV     PIC  9(01)  VALUE ZERO.
     03  FG-NFTENMS-INV     PIC  9(01)  VALUE ZERO.
     03  FG-NFHACPF-INV     PIC  9(01)  VALUE ZERO.
     03  FG-HTOKMS-INV      PIC  9(01)  VALUE ZERO.
*読込・書込カウント領域
 01  CNT-AREA.
     03  IN-NFHAKOF         PIC  9(07)  VALUE ZERO.
     03  IN-NFSUTEF         PIC  9(07)  VALUE ZERO.
     03  SL-NFHAKOF         PIC  9(07)  VALUE ZERO.
     03  SL-NFSUTEF         PIC  9(07)  VALUE ZERO.
     03  OT-NFLISTF1        PIC  9(07)  VALUE ZERO.
     03  OT-NFLISTF2        PIC  9(07)  VALUE ZERO.

*マッチングキー（箱数ファイル）
 01  KY-HK3.
     03  KY-HK3-F01         PIC  9(08). *> 管理番号
     03  KY-HK3-F06         PIC  9(05). *> 店舗ＣＤ
     03  KY-HK3-F07         PIC  9(01). *> 納品場所
     03  KY-HK3-F08         PIC  9(08). *> 店着日
     03  KY-HK3-F05         PIC  X(02). *> 作場ＣＤ
*マッチングキー（数量訂正ファイル）
 01  KY-ST3.
     03  KY-ST3-F01         PIC  9(08). *> 管理番号
     03  KY-ST3-F06         PIC  9(05). *> 店舗ＣＤ
     03  KY-ST3-F07         PIC  9(01). *> 納品場所
     03  KY-ST3-F08         PIC  9(08). *> 店着日
     03  KY-ST3-F05         PIC  X(02). *> 作場ＣＤ

 01  DATE-AREA.
     03  WK-DATE            PIC  9(06).
     03  SYS-DATE           PIC  9(08).
 01  WK-TIME.
     03  SYS-TIME           PIC  9(06).

*出荷単位
 01  WK-SF2-F01             PIC  9(04)  VALUE ZERO.
*帳票連番
 01  WK-SF2-F03             PIC  9(04)  VALUE ZERO.
*店舗マスタ退避
 01  WK-TEN-AREA.
     03  WK-TEN-F01         PIC  9(08)  VALUE ZERO.
     03  WK-TEN-F02         PIC  9(05)  VALUE ZERO.

*抽出条件
 01  WK-RD-KANRNO           PIC  9(08).
 01  TBL-AREA.
     03  TBL-SAKBCD-AREA    OCCURS 20  INDEXED BY TBL-IX.
        05  TBL-SAKBCD      PIC  X(02).

*送り状データ編集用ワーク
**     COPY     NFLISTF1
**              DISJOINING XXX JOINING WSF1 AS PREFIX.
     COPY  NFLISTF1   OF XFDLIB
           JOINING  WSF1  AS PREFIX.
*納品明細書データ編集用ワーク
**     COPY     NFLISTF2
**              DISJOINING XXX JOINING WSF2 AS PREFIX.
     COPY  NFLISTF2   OF XFDLIB
           JOINING  WSF2  AS PREFIX.

*日付変換サブルーチン
 01  SKYDTCKB-AREA.
     03  SKYDTCKB-IN-KBN          PIC  X(01).
     03  SKYDTCKB-IN-YMD6         PIC  9(06).
     03  SKYDTCKB-IN-YMD8         PIC  9(08).
     03  SKYDTCKB-OUT-RET         PIC  X(01).
     03  SKYDTCKB-OUT-YMD         PIC  9(08).
*------------------------------------------------------------*
 LINKAGE              SECTION.
*------------------------------------------------------------*
* 入力パラメータ
 01  PA-TANCD     PIC  X(02). *> 担当者ＣＤ
 01  PA-KBN       PIC  9(01). *> 1:オンライン 2:手書き
 01  PA-KANRNO    PIC  9(08). *> 管理番号
 01  PA-SAKBCD    PIC  X(40). *> 作場ＣＤ  X(2) OCCURS 20
*
**************************************************************
 PROCEDURE             DIVISION
                               USING   PA-TANCD
                                       PA-KBN
                                       PA-KANRNO
                                       PA-SAKBCD.
**************************************************************
 DECLARATIVES.
 HK3-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE NFHAKOF.
     DISPLAY     HK3-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     HK3-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 ST3-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE NFSUTEF.
     DISPLAY     ST3-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ST3-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 KH1-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE NFJOHOF.
     DISPLAY     KH1-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     KH1-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 SKB-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE SAKUBAF.
     DISPLAY     SKB-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     SKB-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 SYO-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE NFSHOMS.
     DISPLAY     SYO-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     SYO-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 TEN-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE NFTENMS.
     DISPLAY     TEN-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     TEN-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 HC2-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE NFHACPF.
     DISPLAY     HC2-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     HC2-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 TOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HTOKMS.
     DISPLAY     TOK-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     TOK-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 SF1-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE NFLISTF1.
     DISPLAY     SF1-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     SF1-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 SF2-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE NFLISTF2.
     DISPLAY     SF2-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     SF2-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 END  DECLARATIVES.
****************************************************************
*             MAIN        MODULE                     0.0       *
****************************************************************
 PROCESS-START         SECTION.
     MOVE  "PROCESS-START"  TO  S-NAME.

     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC
              UNTIL  FG-END = "END".
     PERFORM  END-SEC.

     STOP RUN.
 PROCESS-END.
     EXIT.
****************************************************************
*             初期処理                               1.0       *
****************************************************************
 INIT-SEC              SECTION.
     MOVE  "INIT-SEC"       TO  S-NAME.

*TEST<<<<<
*### DISPLAY  "担当者 ----- " PA-TANCD  UPON CONS.
*### DISPLAY  "区分 ------- " PA-KBN    UPON CONS.
*### DISPLAY  "管理番号 --- " PA-KANRNO UPON CONS.
*### DISPLAY  "作場 ------- " PA-SAKBCD UPON CONS.
*### DISPLAY  " "       UPON CONS.
*TEST>>>>>

* ファイルのＯＰＥＮ
     OPEN  INPUT  NFHAKOF.
     OPEN  INPUT  NFSUTEF.
     OPEN  INPUT  NFJOHOF.
     OPEN  INPUT  SAKUBAF.
     OPEN  INPUT  NFSHOMS.
     OPEN  INPUT  NFTENMS.
     OPEN  INPUT  NFHACPF.
     OPEN  INPUT  HTOKMS.
     OPEN  OUTPUT NFLISTF1.
     OPEN  OUTPUT NFLISTF2.

* システム日付取得
     ACCEPT  WK-DATE  FROM DATE.
     ACCEPT  WK-TIME  FROM TIME.
     MOVE  "3"              TO  SKYDTCKB-IN-KBN.
     MOVE  WK-DATE          TO  SKYDTCKB-IN-YMD6.
     MOVE  ZERO             TO  SKYDTCKB-IN-YMD8.
     MOVE  ZERO             TO  SKYDTCKB-OUT-RET.
     MOVE  ZERO             TO  SKYDTCKB-OUT-YMD.
     CALL  "SKYDTCKB"  USING SKYDTCKB-IN-KBN
                             SKYDTCKB-IN-YMD6
                             SKYDTCKB-IN-YMD8
                             SKYDTCKB-OUT-RET
                             SKYDTCKB-OUT-YMD.
     MOVE  SKYDTCKB-OUT-YMD TO  SYS-DATE.

* 初期値設定
     INITIALIZE              WK-TEN-AREA.
     MOVE  PA-KANRNO        TO  WK-RD-KANRNO.
     MOVE  PA-SAKBCD        TO  TBL-AREA.

* 数量訂正ファイルの初期読み込み
     MOVE  LOW-VALUE        TO  FG-NFSUTEF-END.
     MOVE  PA-KANRNO        TO  ST3-F01.  *> 管理番号
     MOVE  ZERO             TO  ST3-F06.  *> 店舗ＣＤ
     MOVE  SPACE            TO  ST3-F07.  *> 納品場所
     MOVE  ZERO             TO  ST3-F08.  *> 店着日
     MOVE  SPACE            TO  ST3-F05.  *> 作場ＣＤ
     MOVE  ZERO             TO  ST3-F09.  *> 伝票番号
     PERFORM  RD-NFSUTEF-SEC.
     IF  FG-NFSUTEF-END = "END"
         MOVE  "END"     TO  FG-END
         GO TO  INIT-EXIT
     END-IF.

* 箱数ファイルの初期読み込み
     MOVE  LOW-VALUE        TO  FG-NFHAKOF-END.
     MOVE  PA-KANRNO        TO  HK3-F01. *> 管理番号
     MOVE  ZERO             TO  HK3-F06. *> 店舗ＣＤ
     MOVE  ZERO             TO  HK3-F07. *> 納品場所
     MOVE  ZERO             TO  HK3-F08. *> 店着日
     MOVE  SPACE            TO  HK3-F05. *> 作場ＣＤ
     PERFORM  RD-NFHAKOF-SEC.

* 初期値設定
     PERFORM  SET-INIT-SEC.

 INIT-EXIT.
     EXIT.
****************************************************************
*  数量訂正ファイル・読込み
****************************************************************
 RD-NFSUTEF-SEC              SECTION.
     MOVE  "RD-NFSUTEF-SEC"  TO  S-NAME.
*
     IF  FG-NFSUTEF-END = LOW-VALUE
         START  NFSUTEF  KEY >=  ST3-F01
                                 ST3-F06
                                 ST3-F07
                                 ST3-F08
                                 ST3-F05
                                 ST3-F09
           INVALID KEY
              MOVE  "END"       TO  FG-NFSUTEF-END
              MOVE  HIGH-VALUE  TO  KY-ST3
              GO TO  RD-NFSUTEF-EXIT
         END-START

         MOVE  SPACE        TO  FG-NFSUTEF-END

     END-IF.
*
     READ  NFSUTEF
       AT  END
         MOVE  "END"        TO  FG-NFSUTEF-END
         MOVE  HIGH-VALUE   TO  KY-ST3
         GO TO  RD-NFSUTEF-EXIT
     END-READ.
     ADD  1   TO  IN-NFSUTEF.

*管理番号チェック
     IF  ST3-F01 > WK-RD-KANRNO
         MOVE  "END"        TO  FG-NFSUTEF-END
         MOVE  HIGH-VALUE   TO  KY-ST3
         GO TO  RD-NFSUTEF-EXIT
     END-IF.
*作場ＣＤチェック
     IF  PA-SAKBCD = SPACE
         CONTINUE
     ELSE
         SET  TBL-IX  TO  1
         SEARCH  TBL-SAKBCD-AREA
             AT END
                   GO TO  RD-NFSUTEF-SEC
             WHEN  TBL-SAKBCD(TBL-IX) = ST3-F05
                   CONTINUE
         END-SEARCH
     END-IF.
*連携確定区分チェック
     IF   ST3-F98 = "1"
         CONTINUE
     ELSE
         GO TO  RD-NFSUTEF-SEC
     END-IF.

* マッチングキーの編集
     MOVE  ST3-F01          TO  KY-ST3-F01.
     MOVE  ST3-F06          TO  KY-ST3-F06.
     MOVE  ST3-F07          TO  KY-ST3-F07.
     MOVE  ST3-F08          TO  KY-ST3-F08.
     MOVE  ST3-F05          TO  KY-ST3-F05.

     ADD  1   TO  SL-NFSUTEF.

 RD-NFSUTEF-EXIT.
     EXIT.
****************************************************************
*  箱数ファイル・読込み
****************************************************************
 RD-NFHAKOF-SEC              SECTION.
     MOVE  "RD-NFHAKOF-SEC"  TO  S-NAME.
*
     IF  FG-NFHAKOF-END = LOW-VALUE
         START  NFHAKOF  KEY >=  HK3-F01
                                 HK3-F06
                                 HK3-F07
                                 HK3-F08
                                 HK3-F05
           INVALID KEY
             MOVE  "END"       TO  FG-NFHAKOF-END
             MOVE  HIGH-VALUE  TO  KY-HK3
             GO TO  RD-NFHAKOF-EXIT
         END-START

         MOVE  SPACE        TO  FG-NFHAKOF-END

     END-IF.
*
     READ  NFHAKOF  NEXT
       AT  END
         MOVE  "END"        TO  FG-NFHAKOF-END
         MOVE  HIGH-VALUE   TO  KY-HK3
         GO TO  RD-NFHAKOF-EXIT
     END-READ.
     ADD  1   TO  IN-NFHAKOF.

*管理番号
     IF  HK3-F01 > WK-RD-KANRNO
         MOVE  "END"        TO  FG-NFHAKOF-END
         MOVE  HIGH-VALUE   TO  KY-HK3
         GO TO  RD-NFHAKOF-EXIT
     END-IF.
*作場ＣＤ
     IF  PA-SAKBCD = SPACE
         CONTINUE
     ELSE
         SET  TBL-IX  TO  1
         SEARCH  TBL-SAKBCD-AREA
             AT END
                   GO TO  RD-NFHAKOF-SEC
             WHEN  TBL-SAKBCD(TBL-IX) = HK3-F05
                   CONTINUE
         END-SEARCH
     END-IF.
*連携確定区分チェック
     IF   HK3-F98 = "1"
         CONTINUE
     ELSE
         GO TO  RD-NFHAKOF-SEC
     END-IF.
*# 2020/05/26 NAV ST 出荷総梱包数＝０の時は対象としない
*出荷総梱包数チェック
*****IF   HK3-F09 > ZERO
*        CONTINUE
*    ELSE
*        GO TO  RD-NFHAKOF-SEC
*****END-IF.
*# 2020/05/26 NAV ED 出荷総梱包数＝０の時は対象としない

* マッチングキーの編集
     MOVE  HK3-F01          TO  KY-HK3-F01.
     MOVE  HK3-F06          TO  KY-HK3-F06.
     MOVE  HK3-F07          TO  KY-HK3-F07.
     MOVE  HK3-F08          TO  KY-HK3-F08.
     MOVE  HK3-F05          TO  KY-HK3-F05.

     ADD  1   TO  SL-NFHAKOF.

 RD-NFHAKOF-EXIT.
     EXIT.
****************************************************************
*  初期値設定
****************************************************************
 SET-INIT-SEC              SECTION.
     MOVE  "SET-INIT-SEC"  TO  S-NAME.

     MOVE  SPACE            TO  WSF1-REC.
     INITIALIZE                 WSF1-REC.
     MOVE  ","              TO  WSF1-C01
                                WSF1-C02
                                WSF1-C03
                                WSF1-C04
                                WSF1-C05
                                WSF1-C06
                                WSF1-C07
                                WSF1-C08
                                WSF1-C09
                                WSF1-C10
                                WSF1-C11
                                WSF1-C12
                                WSF1-C131
                                WSF1-C132
                                WSF1-C133
                                WSF1-C14
                                WSF1-C15
                                WSF1-C16
                                WSF1-C17
                                WSF1-C18
                                WSF1-C19.
     MOVE  X"28"            TO  WSF1-S061
                                WSF1-S071
                                WSF1-S091
                                WSF1-S111
                                WSF1-S1311
                                WSF1-S1321
                                WSF1-S1331
                                WSF1-S151
                                WSF1-S161
                                WSF1-S191.
     MOVE  X"29"            TO  WSF1-S062
                                WSF1-S072
                                WSF1-S092
                                WSF1-S112
                                WSF1-S1312
                                WSF1-S1322
                                WSF1-S1332
                                WSF1-S152
                                WSF1-S162
                                WSF1-S192.
     MOVE  SPACE            TO  WSF2-REC.
     INITIALIZE                 WSF2-REC.
     MOVE  ","              TO  WSF2-C01
                                WSF2-C02
                                WSF2-C03
                                WSF2-C04
                                WSF2-C05
                                WSF2-C06
                                WSF2-C07
                                WSF2-C08
                                WSF2-C09
                                WSF2-C10
                                WSF2-C11
                                WSF2-C12
                                WSF2-C13
                                WSF2-C14
                                WSF2-C15
                                WSF2-C16
                                WSF2-C17
                                WSF2-C18
                                WSF2-C19
                                WSF2-C20
                                WSF2-C99.
     MOVE  X"28"            TO  WSF2-S071
                                WSF2-S081
                                WSF2-S091
                                WSF2-S121
                                WSF2-S171
                                WSF2-S181.
     MOVE  X"29"            TO  WSF2-S072
                                WSF2-S082
                                WSF2-S092
                                WSF2-S122
                                WSF2-S172
                                WSF2-S182.

*取引先マスタ・索引
     MOVE  99992920         TO  TOK-F01. *> 取引先ＣＤ
     PERFORM  RD-HTOKMS-SEC.
     IF  FG-HTOKMS-INV = 0
*  出荷業務仕入先名
         MOVE  TOK-F02      TO  WSF1-F15
*  出荷業務仕入先住所
         MOVE  TOK-F06      TO  WSF1-F16
         MOVE  TOK-F07      TO  WSF1-F16(16:15)
*  出荷業務仕入先電話番号
         MOVE  TOK-F08      TO  WSF1-F17
*  出荷業務仕入先名
         MOVE  TOK-F02      TO  WSF2-F12
     END-IF.

*記事２
     MOVE  NC"サカタのタネ" TO  WSF1-F19.
*帳票区分
     MOVE  "1"              TO  WSF1-F02.
     MOVE  "2"              TO  WSF2-F02.


 SET-INIT-EXIT.
     EXIT.
****************************************************************
*  メイン処理                                        2.0       *
****************************************************************
 MAIN-SEC               SECTION.
     MOVE  "MAIN-SEC"   TO  S-NAME.

     EVALUATE  TRUE
*******
       WHEN  KY-HK3 < KY-ST3
                       *> "＜"：箱数有り、数量訂正Ｆ無し
         PERFORM  UNTIL FG-NFHAKOF-END = "END"
                     OR KY-HK3 >= KY-ST3
           PERFORM  RD-NFHAKOF-SEC

         END-PERFORM
*******
       WHEN  KY-HK3 = KY-ST3
                       *> "＝"：マッチングしたの場合

*    出荷単位・採番
         ADD    1         TO  WK-SF2-F01
         MOVE   ZERO      TO  WK-SF2-F03

         PERFORM  UNTIL FG-NFSUTEF-END = "END"
                     OR KY-ST3 > KY-HK3
*      ナフコ納品明細書データ編集・出力
           PERFORM  EDWT-MEISAISYO-SEC
*      ナフコ送り状データ編集
           PERFORM  EDWT-OKURIJOU-SEC
*      数量訂正ファイル読み込み
           PERFORM  RD-NFSUTEF-SEC
         END-PERFORM

*      ナフコ送り状データ出力
*# 2020/05/26 NAV ST 箱Ｆ　出荷総梱包数＝０は出力しない
*********IF   HK3-F09 > ZERO
*# 2020/06/01 NAV ST 箱Ｆ　トレー数＝０は出力しない様に変更
         IF   HK3-F11 > ZERO
              WRITE  SF1-REC
              ADD    1         TO  OT-NFLISTF1
         END-IF

*    箱数ファイル読み込み
         PERFORM  RD-NFHAKOF-SEC
*******
       WHEN  KY-HK3 > KY-ST3
                       *> "＞"：箱数無し、数量訂正Ｆ有り
        DISPLAY "SSY3915V NFHAKOF RECORD NOT FOUND KEY="
          KY-ST3 "*"  UPON CONS
          MOVE  "4000"      TO  PROGRAM-STATUS
          STOP RUN

     END-EVALUATE.

     IF  FG-NFSUTEF-END = "END"
         MOVE  "END"        TO  FG-END
     END-IF.

 MAIN-EXIT.
     EXIT.
****************************************************************
*  ナフコ納品明細書データ編集・出力
****************************************************************
 EDWT-MEISAISYO-SEC              SECTION.
     MOVE  "EDWT-MEISAISYO-SEC"  TO  S-NAME.

*帳票連番のカウントアップ
     ADD   1                TO  WK-SF2-F03.

*基本情報マスタ・索引
     MOVE  ST3-F01          TO  KH1-F01. *> 管理番号
     MOVE  ST3-F05          TO  KH1-F05. *> 作場ＣＤ
     MOVE  ST3-F06          TO  KH1-F06. *> 店舗ＣＤ
     MOVE  ST3-F09          TO  KH1-F07. *> 伝票番号
     MOVE  01               TO  KH1-F08. *> 行番号
     MOVE  ST3-F08          TO  KH1-F09. *> 納品日
     PERFORM  RD-NFJOHOF-SEC.
     IF  FG-NFJOHOF-INV = 1
         DISPLAY "SSY3915V NFJOHOF RECORD NOT FOUND KEY="
                 ST3-F01
             "," ST3-F05
             "," ST3-F06
             "," ST3-F09
             "," "01"
             "," ST3-F08
             "*"  UPON CONS
               MOVE  "4000"     TO  PROGRAM-STATUS
               EXIT PROGRAM
     END-IF.

*店舗マスタ・索引
     IF  KH1-HE13  NOT =  WK-TEN-F01
      OR HK3-F06   NOT =  WK-TEN-F02
         MOVE  KH1-HE13     TO  TEN-F01  *> 取引先ＣＤ
                                WK-TEN-F01
         MOVE  HK3-F06      TO  TEN-F02  *> 店舗ＣＤ
                                WK-TEN-F02
         PERFORM  RD-NFTENMS-SEC
     END-IF.

*作場マスタ・索引
     MOVE  HK3-F05          TO  SKB-F01.
     PERFORM  RD-SAKUBAF-SEC.


*レコード編集
*  初期値設定
     MOVE  WSF2-REC         TO  SF2-REC.

*  出荷単位
     MOVE  WK-SF2-F01       TO  SF2-F01.
*  帳票連番
     MOVE  WK-SF2-F03       TO  SF2-F03.

*  直送区分
     IF  FG-SAKUBAF-INV = 0
         MOVE  NC"直送"     TO  SF2-F09
     ELSE
         MOVE  NC"通常"     TO  SF2-F09
     END-IF.

*  納品場所名漢字
     IF  FG-NFTENMS-INV = 0
         MOVE  TEN-F22      TO  SF2-F08
     END-IF.

*箱数ファイルより項目設定 ***********************************
*  納品先店舗コード
     MOVE  HK3-F06          TO  SF2-F06.
*  個口数
     MOVE  HK3-F09          TO  SF2-F10.

*数量訂正ファイルより項目設定 *******************************
*  納品日
     MOVE  ST3-F08          TO  SF2-F04.
*  伝票番号
     MOVE  ST3-F09          TO  SF2-F14.
*  ＪＡＮコード
     MOVE  ST3-F10          TO  SF2-F15.
*  納品数
     MOVE  ST3-F11          TO  SF2-F19.


*基本情報ファイルから項目設定 *******************************
*  出荷NO
     MOVE  KH1-F42          TO  SF2-F11.
*  商品コード
     MOVE  KH1-F13          TO  SF2-F16.


     IF  PA-KBN = 1  *> オンライン

*ナフコ発注累積データから項目設定 ***************************
*  データ・索引
         MOVE  KH1-HE05      TO  HC2-A06  *> 法人ＣＤ
         MOVE  KH1-F06       TO  HC2-A83  *> 納品先店舗ＣＤ
         MOVE  HK3-F07       TO  HC2-A88  *> 納品場所ＣＤ
         MOVE  "3"           TO  SKYDTCKB-IN-KBN
         MOVE  KH1-HE12      TO  SKYDTCKB-IN-YMD6
         MOVE  ZERO          TO  SKYDTCKB-IN-YMD8
         MOVE  ZERO          TO  SKYDTCKB-OUT-RET
         MOVE  ZERO          TO  SKYDTCKB-OUT-YMD
         CALL  "SKYDTCKB"  USING SKYDTCKB-IN-KBN
                                 SKYDTCKB-IN-YMD6
                                 SKYDTCKB-IN-YMD8
                                 SKYDTCKB-OUT-RET
                                 SKYDTCKB-OUT-YMD
         MOVE  SKYDTCKB-OUT-YMD
                             TO  HC2-A26  *> 納品予定日
         MOVE  KH1-F07       TO  HC2-A23  *> 伝票番号
         MOVE  KH1-F08       TO  HC2-A24  *> 行番号
         PERFORM  RD-NFHACPF-SEC
         IF  FG-NFHACPF-INV = 1
             DISPLAY "SSY3915V NFHACPF RECORD NOT FOUND KEY="
                     HC2-A06
                 "," HC2-A83
                 "," HC2-A88
                 "," HC2-A26
                 "," HC2-A23
                 "," HC2-A24
                 "*"  UPON CONS
                   MOVE  "4000"     TO  PROGRAM-STATUS
                   EXIT PROGRAM
         END-IF
*  納品先店舗名
         MOVE  HC2-A85          TO  SF2-F07
*  品名漢字
         MOVE  HC2-A49          TO  SF2-F17
*  規格名漢字
         MOVE  HC2-A53          TO  SF2-F18
*  原単価
         IF  HC2-A62  =  0
             MOVE  HC2-A63      TO  SF2-F20
         ELSE
             MOVE  HC2-A64      TO  SF2-F20
         END-IF

     ELSE             *> 手書き

*店舗マスタより項目設定 *************************************
         IF  FG-NFTENMS-INV = 0
*  納品先店舗名
             MOVE  TEN-F05      TO  SF2-F07
         ELSE
             MOVE  SPACE        TO  SF2-F07
         END-IF

*ナフコ商品マスタ・索引・項目設定 ***************************
         MOVE  KH1-F13        TO  SYO-F01
         PERFORM  RD-NFSHOMS-SEC
         IF  FG-NFSHOMS-INV = 0
*  品名漢字
             MOVE  SYO-F05    TO  SF2-F17
*  規格名漢字
             MOVE  SYO-F06    TO  SF2-F18
         END-IF

*  原単価
         COMPUTE  SF2-F20  =  KH1-ME10 * 100

     END-IF.

*出力
*# 2020/05/26 NAV ST 出荷総梱包数＝０の時は対象としない
*出荷総梱包数チェック
*# 2020/06/01 NAV ST トレー数＝０の時は対象としない
*****IF   HK3-F09 > ZERO
     IF   HK3-F11 > ZERO
         CONTINUE
     ELSE
         GO TO  EDWT-MEISAISYO-EXIT
     END-IF.
*# 2020/05/26 NAV ED 出荷総梱包数＝０の時は対象としない
     WRITE SF2-REC.
     ADD   1                TO  OT-NFLISTF2.
*
 EDWT-MEISAISYO-EXIT.
     EXIT.
****************************************************************
*  ナフコ送り状データ編集
****************************************************************
 EDWT-OKURIJOU-SEC               SECTION.
     MOVE  "EDWT-OKURIJOU-SEC"   TO  S-NAME.

     IF  WK-SF2-F01  NOT =  WSF1-F01
*  出荷単位
         MOVE  WK-SF2-F01   TO  WSF1-F01
         MOVE  WSF1-REC     TO  SF1-REC

*  納品先店舗コード
         MOVE  HK3-F06      TO  SF1-F05
*  納品日
         MOVE  HK3-F08      TO  SF1-F10
*  個口数
         MOVE  HK3-F09      TO  SF1-F12
*  記事１
         MOVE  HK3-F05      TO  SF1-F18

*  出荷NO
         MOVE  KH1-F42      TO  SF1-F04
*  取引先ＣＤ
         MOVE  KH1-HE13     TO  SF1-F14

*  直送区分
         IF  FG-SAKUBAF-INV = 0
             MOVE  NC"直送"     TO  SF1-F11
         ELSE
             MOVE  NC"通常"     TO  SF1-F11
         END-IF

*  納品先店舗名
         IF  PA-KBN = 1  *> オンライン
             MOVE  HC2-A85      TO  SF1-F06
         ELSE             *> 手書き
             IF  FG-NFTENMS-INV = 0
                 MOVE  TEN-F05  TO  SF1-F06
             END-IF
         END-IF

         IF  FG-NFTENMS-INV = 0
*  納品場所名漢字
             MOVE  TEN-F22      TO  SF1-F07
*  店舗電話番号
             MOVE  TEN-F10      TO  SF1-F08
*  店舗住所
             MOVE  TEN-F08      TO  SF1-F09
             MOVE  TEN-F09      TO  SF1-F09(16:15)
         END-IF

     END-IF.


*  発注形態１
     IF  ST3-FIL(1:2)  =  "40"
         MOVE   NC"○"          TO  SF1-F131
     END-IF.
*  発注形態２
     IF  ST3-FIL(1:2)  =  "30"
         MOVE   NC"○"          TO  SF1-F132
     END-IF.
*  発注形態３
     IF  ST3-FIL(1:2)  =  "20"
         MOVE   NC"○"          TO  SF1-F133
     END-IF.

 EDWT-OKURIJOU-EXIT.
     EXIT.
****************************************************************
*    作場マスタ検索
****************************************************************
 RD-SAKUBAF-SEC              SECTION.
     MOVE  "RD-SAKUBAF-SEC"  TO  S-NAME.
*
     READ  SAKUBAF
       INVALID
         MOVE  1                 TO  FG-SAKUBAF-INV
       NOT INVALID
         MOVE  ZERO              TO  FG-SAKUBAF-INV
     END-READ.
*
 RD-SAKUBAF-EXIT.
     EXIT.
****************************************************************
*    ナフコ商品マスタ検索
****************************************************************
 RD-NFSHOMS-SEC             SECTION.
     MOVE  "RD-NFSHOMS-SEC" TO  S-NAME.
*
     READ  NFSHOMS
       INVALID
         MOVE  1                 TO  FG-NFSHOMS-INV
       NOT INVALID
         MOVE  ZERO              TO  FG-NFSHOMS-INV
     END-READ.
*
 RD-NFSHOMS-EXIT.
     EXIT.
****************************************************************
*    基本情報ファイル検索
****************************************************************
 RD-NFJOHOF-SEC             SECTION.
     MOVE  "RD-NFJOHOF-SEC" TO  S-NAME.
*
     READ  NFJOHOF
       INVALID
         MOVE  1                 TO  FG-NFJOHOF-INV
       NOT INVALID
         MOVE  ZERO              TO  FG-NFJOHOF-INV
     END-READ.
*
 RD-NFJOHOF-EXIT.
     EXIT.
****************************************************************
*    ナフコ店舗マスタ検索
****************************************************************
 RD-NFTENMS-SEC             SECTION.
     MOVE  "RD-NFTENMS-SEC" TO  S-NAME.
*
     READ  NFTENMS
       INVALID
         MOVE  1                 TO  FG-NFTENMS-INV
       NOT INVALID
         MOVE  ZERO              TO  FG-NFTENMS-INV
     END-READ.
*
 RD-NFTENMS-EXIT.
     EXIT.
****************************************************************
*    ナフコ発注累積データ検索
****************************************************************
 RD-NFHACPF-SEC             SECTION.
     MOVE  "RD-NFHACPF-SEC" TO  S-NAME.
*
     READ  NFHACPF
       INVALID
         MOVE  1                 TO  FG-NFHACPF-INV
       NOT INVALID
         MOVE  ZERO              TO  FG-NFHACPF-INV
     END-READ.
*
 RD-NFHACPF-EXIT.
     EXIT.
****************************************************************
*    取引先マスタ検索
****************************************************************
 RD-HTOKMS-SEC              SECTION.
     MOVE  "RD-HTOKMS-SEC"  TO  S-NAME.
*
     READ  HTOKMS
       INVALID
         MOVE  1                 TO  FG-HTOKMS-INV
       NOT INVALID
         MOVE  ZERO              TO  FG-HTOKMS-INV
     END-READ.
*
 RD-HTOKMS-EXIT.
     EXIT.
****************************************************************
*  終了処理                                          3.0       *
****************************************************************
 END-SEC                  SECTION.
     MOVE  "END-SEC"      TO  S-NAME.
*
*TEST<<<<<
*####     DISPLAY  "NFHAKOF  IN = " IN-NFHAKOF UPON CONS.
*####     DISPLAY  "NFHAKOF  SEL= " SL-NFHAKOF UPON CONS.
*####     DISPLAY  "NFSUTEF  IN = " IN-NFSUTEF UPON CONS.
*####     DISPLAY  "NFSUTEF  SEL= " SL-NFSUTEF UPON CONS.
*####     DISPLAY  "KEY-BREAK   = " OT-BREAK   UPON CONS.
*TEST>>>>>
     DISPLAY  "NFLISTF1 OT = "  OT-NFLISTF1  UPON CONS.
     DISPLAY  "NFLISTF2 OT = "  OT-NFLISTF2  UPON CONS.

* ファイルのＯＰＥＮ
     CLOSE  NFHAKOF.
     CLOSE  NFSUTEF.
     CLOSE  NFJOHOF.
     CLOSE  SAKUBAF.
     CLOSE  NFSHOMS.
     CLOSE  NFTENMS.
     CLOSE  NFHACPF.
     CLOSE  HTOKMS.
     CLOSE  NFLISTF1.
     CLOSE  NFLISTF2.

 END-EXIT.
     EXIT.
*****************<<  SSY3915V   END PROGRAM  >>******************

```
