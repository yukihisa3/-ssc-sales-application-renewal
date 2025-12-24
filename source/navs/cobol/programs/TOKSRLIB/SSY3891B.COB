***********************************************************
*    顧客名　　　　　：（株）サカタのタネ殿　　　　       *
*    サブシステム　　：ナフコＥＤＩ受信システム　         *
*    業務名　　　　　：ナフコＥＤＩ受信                   *
*    モジュール名　　：ナフコ出荷（ＡＳＮ）データ作成     *
*    作成日／作成者　：2019/12/05    ASS.TAKAHASHI        *
*    処理概要　　　　：                                   *
*      箱数ファイル、数量訂正ファイルよりナフコ出荷（ＡＳ *
*      Ｎ）を作成する。                                   *
*    更新日／更新者　：2020/03/26                         *
*    更新内容　　　　：納品場所名、納品場所名カナ転送変更 *
*    更新日／更新者　：2020/04/06                         *
*    更新内容　　　　：直送先セット方法変更　　　　　　　 *
***********************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SSY3891B.
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

* 条件ファイル
     SELECT  HJYOKEN
       ASSIGN         TO  JYOKEN1
       ORGANIZATION   IS  INDEXED
       ACCESS MODE    IS  RANDOM
       RECORD KEY     IS  JYO-F01   *> ＫＥＹ１
                          JYO-F02   *> ＫＥＹ２
       FILE STATUS    IS  JYO-ST.

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

* ナフコＡＳＮデータ（ヘッダ）
     SELECT  NFASNSF1
       ASSIGN         TO  DA-01-S-NFASNSF1
       FILE STATUS    IS  SF1-ST.

* ナフコＡＳＮデータ（明細）
     SELECT  NFASNSF2
       ASSIGN         TO  DA-01-S-NFASNSF2
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
*    FILE = 条件ファイル                                       *
****************************************************************
 FD  HJYOKEN
                       LABEL     RECORD    IS   STANDARD.
**     COPY     HJYOKEN
**              DISJOINING XXX JOINING JYO AS PREFIX.
                       COPY      HJYOKEN   OF   XFDLIB
                       JOINING   JYO       AS   PREFIX.
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
*    FILE = ナフコＡＳＮデータ（ヘッダ）                       *
****************************************************************
 FD  NFASNSF1          BLOCK     CONTAINS  1    RECORDS
                       LABEL     RECORD    IS   STANDARD.
**     COPY     NFASNSF1
**              DISJOINING XXX JOINING SF1 AS PREFIX.
                       COPY      NFASNSF1  OF   XFDLIB
                       JOINING   SF1       AS   PREFIX.
****************************************************************
*    FILE = ナフコＡＳＮデータ（明細）                         *
****************************************************************
 FD  NFASNSF2          BLOCK     CONTAINS  1    RECORDS
                       LABEL     RECORD    IS   STANDARD.
**     COPY     NFASNSF2
**              DISJOINING XXX JOINING SF2 AS PREFIX.
                       COPY      NFASNSF2  OF   XFDLIB
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
     03  JYO-ST             PIC  X(02).
     03  TEN-ST             PIC  X(02).
     03  HC2-ST             PIC  X(02).
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
     03  JYO-ERR           PIC N(20) VALUE
         NC"条件ファイル１・エラー".
     03  TEN-ERR           PIC N(20) VALUE
         NC"ナフコ店舗マスタ１・エラー".
     03  HC2-ERR           PIC N(20) VALUE
         NC"ナフコ発注累積データ２・エラー".
     03  SF1-ERR           PIC N(20) VALUE
         NC"ナフコＡＳＮデータ（ヘッダ）エラー".
     03  SF2-ERR           PIC N(20) VALUE
         NC"ナフコＡＳＮデータ（明細）エラー".

*読込フラグ領域
 01  FLG-AREA.
     03  FG-END             PIC  X(03)  VALUE SPACE.
     03  FG-NFHAKOF-END     PIC  X(03)  VALUE SPACE.
     03  FG-NFSUTEF-END     PIC  X(03)  VALUE SPACE.
     03  FG-NFJOHOF-INV     PIC  9(01)  VALUE ZERO.
     03  FG-SAKUBAF-INV     PIC  9(01)  VALUE ZERO.
     03  FG-NFSHOMS-INV     PIC  9(01)  VALUE ZERO.
     03  FG-HJYOKEN-INV     PIC  9(01)  VALUE ZERO.
     03  FG-NFTENMS-INV     PIC  9(01)  VALUE ZERO.
     03  FG-NFHACPF-INV     PIC  9(01)  VALUE ZERO.
*読込・書込カウント領域
 01  CNT-AREA.
     03  IN-NFHAKOF         PIC  9(07)  VALUE ZERO.
     03  IN-NFSUTEF         PIC  9(07)  VALUE ZERO.
     03  SL-NFHAKOF         PIC  9(07)  VALUE ZERO.
     03  SL-NFSUTEF         PIC  9(07)  VALUE ZERO.
     03  OT-NFASNSF1        PIC  9(07)  VALUE ZERO.
     03  OT-NFASNSF2        PIC  9(07)  VALUE ZERO.
     03  OT-BREAK           PIC  9(07)  VALUE ZERO.

*マッチングキー（箱数ファイル）
 01  KY-HK3.
     03  KY-HK3-F01         PIC  9(08). *> 管理番号
     03  KY-HK3-F06         PIC  9(05). *> 店舗ＣＤ
     03  KY-HK3-F07         PIC  X(01). *> 納品場所
     03  KY-HK3-F08         PIC  9(08). *> 店着日
     03  KY-HK3-F05         PIC  X(02). *> 作場ＣＤ
*マッチングキー（数量訂正ファイル）
 01  KY-ST3.
     03  KY-ST3-F01         PIC  9(08). *> 管理番号
     03  KY-ST3-F06         PIC  9(05). *> 店舗ＣＤ
     03  KY-ST3-F07         PIC  X(01). *> 納品場所
     03  KY-ST3-F08         PIC  9(08). *> 店着日
     03  KY-ST3-F05         PIC  X(02). *> 作場ＣＤ

 01  DATE-AREA.
     03  WK-DATE            PIC  9(06).
     03  SYS-DATE           PIC  9(08).
 01  WK-TIME.
     03  SYS-TIME           PIC  9(06).

 01  WK-KINGAKU             PIC  9(09)V99.
*管理連番
 01  WK-JYO-F04             PIC  9(05)  VALUE ZERO.
*出荷NO
 01  WK-SYUKANO.
     03  WK-SYUKANO-C06     PIC  9(02). *> 法人ＣＤ
     03  WK-SYUKANO-C07     PIC  9(03). *> 店舗ＣＤ
     03  WK-SYUKANO-C10     PIC  9(06). *> 出荷業務仕入先ＣＤ
     03  WK-SYUKANO-FIL     PIC  9(02). *> ００
     03  WK-SYUKANO-SEQ     PIC  9(05). *> 通番


*抽出条件
 01  WK-RD-KANRNO           PIC  9(08).
 01  TBL-AREA.
     03  TBL-SAKBCD-AREA    OCCURS 20  INDEXED BY TBL-IX.
        05  TBL-SAKBCD      PIC  X(02).

*明細データ編集用ワーク
**     COPY     NFASNSF2
**              DISJOINING XXX JOINING WSF2 AS PREFIX.
     COPY  NFASNSF2   OF XFDLIB
           JOINING  WSF2  AS PREFIX.
*店舗マスタ退避
 01  WTEN-AREA.
     03  WTEN-F01           PIC  9(08).
     03  WTEN-F02           PIC  9(05).
     03  WTEN-F21           PIC  9(02).
     03  WTEN-F22           PIC  N(03).
     03  WTEN-F23           PIC  X(06).
     03  WTEN-F24           PIC  9(05).
*ADD↓
 01  WHAKO-AREA.
     03  WHAKO-F07          PIC  X(01).
     03  WHAKO-F08          PIC  9(08).
     03  WHAKO-F05          PIC  X(02).
*ADD↑

*日付変換サブルーチン
 01  SKYDTCKB-AREA.
     03  SKYDTCKB-IN-KBN          PIC  X(01).
     03  SKYDTCKB-IN-YMD6         PIC  9(06).
     03  SKYDTCKB-IN-YMD8         PIC  9(08).
     03  SKYDTCKB-OUT-RET         PIC  X(01).
     03  SKYDTCKB-OUT-YMD         PIC  9(08).
*消費税率取得サブ
 01  SKYTAXPG-AREA.
     03  SKYTAXPG-IN-ZEIKBN       PIC X(01).
     03  SKYTAXPG-IN-DATE         PIC 9(08).
     03  SKYTAXPG-OUT-ERR         PIC X(01).
     03  SKYTAXPG-OUT-ZEI         PIC 9(05).
     03  SKYTAXPG-OUT-DZEI        PIC 9(05).
*------------------------------------------------------------*
 LINKAGE              SECTION.
*------------------------------------------------------------*
* 入力パラメータ
 01  PA-TANCD     PIC  X(02). *> 担当者ＣＤ
 01  PA-KBN       PIC  9(01). *> 1:オンライン 2:手書き
 01  PA-KANRNO    PIC  9(08). *> 管理番号
 01  PA-SAKBCD    PIC  X(40). *> 作場ＣＤ  X(2) OCCURS 20
 01  PA-SYKYMD    PIC  9(08). *> 出荷日
 01  PA-TENYMD    PIC  9(08). *> 店着日
 01  PA-SYORI     PIC  9(01). *> 1:新規 2:再送 3:再作成
*
**************************************************************
 PROCEDURE             DIVISION
                               USING   PA-TANCD
                                       PA-KBN
                                       PA-KANRNO
                                       PA-SAKBCD
                                       PA-SYKYMD
                                       PA-TENYMD
                                       PA-SYORI.
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
 JYO-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HJYOKEN.
     DISPLAY     JYO-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     JYO-ST    UPON      CONS.
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
 SF1-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE NFASNSF1.
     DISPLAY     SF1-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     SF1-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 SF2-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE NFASNSF2.
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
*### DISPLAY  "出荷日 ----- " PA-SYKYMD UPON CONS.
*### DISPLAY  "店着日 ----- " PA-TENYMD UPON CONS.
*### DISPLAY  "処理モード - " PA-SYORI  UPON CONS.
*### DISPLAY  " "       UPON CONS.
*TEST>>>>>

* ファイルのＯＰＥＮ
     OPEN  I-O    NFHAKOF.
     OPEN  I-O    NFSUTEF.
     OPEN  I-O    NFJOHOF.
     OPEN  INPUT  SAKUBAF.
     OPEN  INPUT  NFSHOMS.
     OPEN  I-O    NFTENMS.
     OPEN  INPUT  NFHACPF.
     OPEN  OUTPUT NFASNSF1.
     OPEN  OUTPUT NFASNSF2.

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
     INITIALIZE              WTEN-AREA.
*ADD↓
     INITIALIZE              WHAKO-AREA.
*ADD↑
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

* 管理連番を採番
     PERFORM  SET-KANRINO-SEC.

* 消費税率を取得
     IF  PA-KBN = 2
         MOVE  "0"            TO  SKYTAXPG-IN-ZEIKBN
         MOVE  SYS-DATE       TO  SKYTAXPG-IN-DATE
         CALL  "SKYTAXPG"  USING  SKYTAXPG-IN-ZEIKBN
                                  SKYTAXPG-IN-DATE
                                  SKYTAXPG-OUT-ERR
                                  SKYTAXPG-OUT-ZEI
                                  SKYTAXPG-OUT-DZEI
*????????  MOVE  1000   TO  SKYTAXPG-OUT-DZEI
     END-IF.

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
*店着日チェック
     IF  PA-TENYMD = ZERO
         CONTINUE
     ELSE
         IF  ST3-F08 NOT = PA-TENYMD
             GO TO  RD-NFSUTEF-SEC
         END-IF
     END-IF.
*出荷日チェック
     IF  PA-SYKYMD = ZERO
         CONTINUE
     ELSE
         IF  ST3-F14 NOT = PA-SYKYMD
             GO TO  RD-NFSUTEF-SEC
         END-IF
     END-IF.
*連携確定区分チェック
     IF   ( PA-SYORI = 1  AND  ST3-F98 = SPACE )
       OR ( PA-SYORI = 2  AND  ST3-F98 = "1"   )
       OR ( PA-SYORI = 3  AND  ST3-F98 = SPACE )
         CONTINUE
     ELSE
         GO TO  RD-NFSUTEF-SEC
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
*店着日
     IF     PA-TENYMD = ZERO
         CONTINUE
     ELSE
         IF  HK3-F08 NOT = PA-TENYMD
             GO TO  RD-NFHAKOF-SEC
         END-IF
     END-IF.
*連携確定区分チェック
     IF   ( PA-SYORI = 1  AND  HK3-F98 = SPACE )
       OR ( PA-SYORI = 2  AND  HK3-F98 = "1"   )
       OR ( PA-SYORI = 3  AND  HK3-F98 = SPACE )
         CONTINUE
     ELSE
         GO TO  RD-NFHAKOF-SEC
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
*  管理連番の取得
****************************************************************
 SET-KANRINO-SEC               SECTION.
     MOVE  "SET-KANRINO-SEC"   TO  S-NAME.

     OPEN  I-O    HJYOKEN.

*検索
     MOVE  99               TO  JYO-F01.
     MOVE  "NAFUKOA"        TO  JYO-F02.
     READ  HJYOKEN
       INVALID
         DISPLAY "SSY3891B HJYOKEN RECORD NOT FOUND KEY="
                 "99,NAFUKOA*"  UPON CONS
               MOVE  "4000"     TO  PROGRAM-STATUS
               EXIT PROGRAM
     END-READ.
* カウントアップ
     IF  JYO-F04  <  JYO-F06
         ADD   1            TO  JYO-F04
     ELSE
         MOVE  JYO-F05      TO  JYO-F04
     END-IF.
     MOVE  JYO-F04          TO  WK-JYO-F04.
*更新
     REWRITE  JYO-REC.

     CLOSE  HJYOKEN.

 SET-KANRINO-EXIT.
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
***      DISPLAY "AAAAA" UPON  CONS
         PERFORM  UNTIL FG-NFHAKOF-END = "END"
                     OR KY-HK3 >= KY-ST3
           PERFORM  RD-NFHAKOF-SEC

         END-PERFORM
*******
       WHEN  KY-HK3 = KY-ST3
                       *> "＝"：マッチングしたの場合

***      DISPLAY "BBBBB" UPON  CONS
*    箱数ファイルから編集
         PERFORM  EDWT-NFHAKDT-SEC

         ADD   1   TO  OT-BREAK
         PERFORM  UNTIL FG-NFSUTEF-END = "END"
                     OR KY-ST3 > KY-HK3
*      ナフコＡＳＮデータ（明細）編集・出力
           PERFORM  EDWT-SEC
*      数量訂正ファイル読み込み
           PERFORM  RD-NFSUTEF-SEC
***       DISPLAY "KY-ST3 = " KY-ST3 UPON CONS
***       DISPLAY "KY-HK3 = " KY-HK3 UPON CONS
         END-PERFORM

*    箱数ファイル読み込み
         PERFORM  RD-NFHAKOF-SEC
*******
       WHEN  KY-HK3 > KY-ST3
                       *> "＞"：箱数無し、数量訂正Ｆ有り
***      DISPLAY "CCCCC" UPON  CONS
        DISPLAY "SSY3891B NFHAKOF RECORD NOT FOUND KEY="
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
*  箱数ファイルからＡＳＮデータ項目セット
****************************************************************
 EDWT-NFHAKDT-SEC              SECTION.
     MOVE  "EDWT-NFHAKDT-SEC"  TO  S-NAME.

*レコード初期化
     MOVE  SPACE            TO  WSF2-REC.
     INITIALIZE                 WSF2-REC.
     MOVE  "C01"            TO  WSF2-C01.
     MOVE  SYS-DATE         TO  WSF2-C02(1:8).
     MOVE  WK-JYO-F04       TO  WSF2-C02(9:5).
     MOVE  SYS-DATE         TO  WSF2-C03
                                WSF2-C04
     MOVE  SYS-TIME         TO  WSF2-C05.

*店舗ＣＤ
     MOVE  HK3-F06          TO  WSF2-C07.
*店着日
     MOVE  HK3-F08          TO  WSF2-C17.
*出荷総個数
     MOVE  HK3-F09          TO  WSF2-C27.
*納品先店舗ＣＤ
     MOVE  HK3-F06          TO  WSF2-C56.
*納品場所ＣＤ
     MOVE  HK3-F07          TO  WSF2-C57.
***  DISPLAY "HK3-F01 = " HK3-F01  UPON CONS.
*    DISPLAY "HK3-F02 = " HK3-F02  UPON CONS.
*    DISPLAY "HK3-F03 = " HK3-F03  UPON CONS.
*    DISPLAY "HK3-F04 = " HK3-F04  UPON CONS.
*    DISPLAY "HK3-F05 = " HK3-F05  UPON CONS.
*    DISPLAY "HK3-F06 = " HK3-F06  UPON CONS.
***  DISPLAY "HK3-F07 = " HK3-F07  UPON CONS.

*制御バイト
     MOVE  X"28"            TO  WSF2-C35
                                WSF2-C39
                                WSF2-C58.
     MOVE  X"29"            TO  WSF2-C37
                                WSF2-C41
                                WSF2-C60.

*箱数ファイル更新（連携区分セット）
     MOVE  "1"              TO  HK3-F98.
     MOVE  SYS-DATE         TO  HK3-F99.
     REWRITE HK3-REC.
*
 EDWT-NFHAKDT-EXIT.
     EXIT.
****************************************************************
*  編集・出力処理
****************************************************************
 EDWT-SEC               SECTION.
     MOVE  "EDWT-SEC"   TO  S-NAME.

*基本情報マスタ・索引
     MOVE  ST3-F01          TO  KH1-F01. *> 管理番号
     MOVE  ST3-F05          TO  KH1-F05. *> 作場ＣＤ
     MOVE  ST3-F06          TO  KH1-F06. *> 店舗ＣＤ
     MOVE  ST3-F09          TO  KH1-F07. *> 伝票番号
     MOVE  01               TO  KH1-F08. *> 行番号
     MOVE  ST3-F08          TO  KH1-F09. *> 納品日
     PERFORM  RD-NFJOHOF-SEC.
     IF  FG-NFJOHOF-INV = 1
         DISPLAY "SSY3891B NFJOHOF RECORD NOT FOUND KEY="
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
     IF  KH1-HE13 NOT =  WTEN-F01
      OR HK3-F06  NOT =  WTEN-F02
*ADD↓
      OR HK3-F07  NOT =  WHAKO-F07
      OR HK3-F08  NOT =  WHAKO-F08
      OR HK3-F05  NOT =  WHAKO-F05
         INITIALIZE         WHAKO-AREA
         MOVE  HK3-F07      TO  WHAKO-F07
         MOVE  HK3-F08      TO  WHAKO-F08
         MOVE  HK3-F05      TO  WHAKO-F05
*ADD↑
         INITIALIZE         WTEN-AREA
         MOVE  KH1-HE13     TO  WTEN-F01
         MOVE  HK3-F06      TO  WTEN-F02

         MOVE  KH1-HE13     TO  TEN-F01  *> 取引先ＣＤ
         MOVE  HK3-F06      TO  TEN-F02  *> 店舗ＣＤ
         PERFORM  RD-NFTENMS-SEC
         IF  FG-NFTENMS-INV = 0
             MOVE  TEN-F21       TO  WTEN-F21
             MOVE  TEN-F22       TO  WTEN-F22
             MOVE  TEN-F23       TO  WTEN-F23
             MOVE  TEN-F24       TO  WTEN-F24

*    出荷ＮＯを編集（新規のみ）
             IF  PA-SYORI = 1
*    通番カウントアップ
                 IF  TEN-F24  <  99999
                     ADD   1     TO  TEN-F24
                 ELSE
                     MOVE  1     TO  TEN-F24
                 END-IF
                 INITIALIZE      WK-SYUKANO
                 MOVE  ST3-F06   TO  WK-SYUKANO-C07
                 MOVE  TEN-F24   TO  WK-SYUKANO-SEQ
                 MOVE  TEN-F24   TO  WTEN-F24
*
*    店舗マスタ更新（通番）
                 REWRITE  TEN-REC
             END-IF

         ELSE
             MOVE  04            TO  WTEN-F21
             IF  PA-SYORI = 1
                 INITIALIZE      WK-SYUKANO
                 MOVE  ST3-F06   TO  WK-SYUKANO-C07
                 MOVE  1         TO  WK-SYUKANO-SEQ
             END-IF

         END-IF
     END-IF.

*作場マスタ・索引
     MOVE  HK3-F05          TO  SKB-F01.
     PERFORM  RD-SAKUBAF-SEC.
     IF  FG-SAKUBAF-INV = 0
         MOVE  1            TO  WSF2-C22
*********IF  HK3-F05  =  "41" OR  "E1"  OR  "AA"
         IF  HK3-F05  =  "41" OR  "E1"
             MOVE  0        TO  WSF2-C22
         END-IF
     ELSE
         MOVE  0            TO  WSF2-C22
     END-IF.

*****
     IF  PA-KBN = 1  *> オンライン
*    ナフコＡＳＮデータ（明細）ＥＤＩ通常発注用
         PERFORM  EDWT-NFSUTDT-SEC
     ELSE             *> 手書き
*    ナフコＡＳＮデータ（明細）ＦＡＸ発注用
         PERFORM  EDWT-NFFAXDT-SEC
     END-IF.
*****

*数量訂正ファイル更新（連携区分セット）
     MOVE  "1"              TO  ST3-F98.
     MOVE  SYS-DATE         TO  ST3-F99.
     REWRITE ST3-REC.

*ナフコ基本情報ファイル更新（連携区分セット）
     MOVE  "1"              TO  KH1-F39.
     IF  PA-SYORI  =  1
         MOVE  WK-SYUKANO   TO  KH1-F42
     END-IF.
     REWRITE KH1-REC.


 EDWT-EXIT.
     EXIT.
****************************************************************
*  編集・出力／ナフコＡＳＮデータ（明細）ＥＤＩ通常発注用
****************************************************************
 EDWT-NFSUTDT-SEC              SECTION.
     MOVE  "EDWT-NFSUTDT-SEC"  TO  S-NAME.

     MOVE  WSF2-REC         TO  SF2-REC.

*数量訂正ファイルより項目設定 *******************************
*  伝票番号
     MOVE  ST3-F09          TO  SF2-C12.
*  発注形態区分
     MOVE  ST3-FIL(1:2)     TO  SF2-C20.
*  ＪＡＮコード
     MOVE  ST3-F10          TO  SF2-C31.
*  納品数量
     COMPUTE  SF2-C44  =  ST3-F11 * 10.
*  訂正理由区分
     IF  ST3-F12 = SPACE
         MOVE  ZERO         TO  SF2-C45
     ELSE
         MOVE  ST3-F12      TO  SF2-C45
     END-IF.
***  DISPLAY "ST3-F12 = " ST3-F12  UPON CONS.
***  DISPLAY "SF2-C45 = " SF2-C45  UPON CONS.

*ナフコ店舗マスから項目設定 *********************************
*  納品場所名漢字
     MOVE  WTEN-F22         TO  SF2-C59.
*  納品場所名カナ
     MOVE  WTEN-F23         TO  SF2-C61.
*基本情報ファイルから項目設定 *******************************
*  法人ＣＤ
     MOVE  KH1-HE05         TO  SF2-C06.
*  部門ＣＤ
*XXXXMOVE  KH1-HE09         TO  SF2-C08.
     MOVE  KH1-MF06         TO  SF2-C08.
*  仕入先ＣＤ
     MOVE  KH1-HE13         TO  SF2-C09.
*  出荷業務仕入先ＣＤ
     MOVE  KH1-HE13         TO  SF2-C10.
*  伝票区分
     MOVE  KH1-HE10         TO  SF2-C11.
*  行番号
     MOVE  KH1-ME03         TO  SF2-C13.
*  発注日
     MOVE  "3"              TO  SKYDTCKB-IN-KBN.
     MOVE  KH1-HE11         TO  SKYDTCKB-IN-YMD6.
     MOVE  ZERO             TO  SKYDTCKB-IN-YMD8.
     MOVE  ZERO             TO  SKYDTCKB-OUT-RET.
     MOVE  ZERO             TO  SKYDTCKB-OUT-YMD.
     CALL  "SKYDTCKB"  USING SKYDTCKB-IN-KBN
                             SKYDTCKB-IN-YMD6
                             SKYDTCKB-IN-YMD8
                             SKYDTCKB-OUT-RET
                             SKYDTCKB-OUT-YMD.
     MOVE  SKYDTCKB-OUT-YMD TO  SF2-C15.
*  納品日
     MOVE  "3"              TO  SKYDTCKB-IN-KBN.
     MOVE  KH1-HE12         TO  SKYDTCKB-IN-YMD6.
     MOVE  ZERO             TO  SKYDTCKB-IN-YMD8.
     MOVE  ZERO             TO  SKYDTCKB-OUT-RET.
     MOVE  ZERO             TO  SKYDTCKB-OUT-YMD.
     CALL  "SKYDTCKB"  USING SKYDTCKB-IN-KBN
                             SKYDTCKB-IN-YMD6
                             SKYDTCKB-IN-YMD8
                             SKYDTCKB-OUT-RET
                             SKYDTCKB-OUT-YMD.
     MOVE  SKYDTCKB-OUT-YMD TO  SF2-C16.
*  出荷日
*XXXXMOVE  KH1-F10          TO  SF2-C19.
     MOVE  ST3-F14          TO  SF2-C19.
*  出荷NO
     IF  PA-SYORI = 1
         MOVE  SF2-C06      TO  WK-SYUKANO-C06
         MOVE  SF2-C10      TO  WK-SYUKANO-C10
         MOVE  WK-SYUKANO   TO  SF2-C24
     ELSE
         MOVE  KH1-F42      TO  SF2-C24
     END-IF.
*  梱包_
     MOVE  ALL "0"          TO  SF2-C26.
*  商品ＣＤ
     MOVE  KH1-F13          TO  SF2-C32.
*  品名カナ
     MOVE  KH1-ME16         TO  SF2-C38.
*  発注数
     COMPUTE  SF2-C43  =  KH1-F19 * 10.

*ナフコ発注累積データから項目設定 ***************************
*  データ・索引
     MOVE  SF2-C06          TO  HC2-A06.  *> 法人ＣＤ
     MOVE  SF2-C07          TO  HC2-A83.  *> 納品先店舗ＣＤ
     MOVE  SF2-C57          TO  HC2-A88.  *> 納品場所ＣＤ
     MOVE  SF2-C16          TO  HC2-A26.  *> 納品予定日
     MOVE  SF2-C12          TO  HC2-A23.  *> 伝票番号
     MOVE  SF2-C13          TO  HC2-A24.  *> 行番号
     PERFORM  RD-NFHACPF-SEC.
     IF  FG-NFHACPF-INV = 1
         DISPLAY "SSY3891B NFHACPF RECORD NOT FOUND KEY="
                 HC2-A06
             "," HC2-A83
             "," HC2-A88
             "," HC2-A26
             "," HC2-A23
             "," HC2-A24
             "*"  UPON CONS
               MOVE  "4000"     TO  PROGRAM-STATUS
               EXIT PROGRAM
     END-IF.
*  オプション使用欄
     MOVE  HC2-A46          TO  SF2-C33.
*  ＧＴＩＮ
     MOVE  HC2-A47          TO  SF2-C34.
*  品名漢字
     MOVE  HC2-A49          TO  SF2-C36.
*  規格名漢字
     MOVE  HC2-A53          TO  SF2-C40.
*  規格名カナ
     MOVE  HC2-A55          TO  SF2-C42.
*  総額取引区分
     MOVE  HC2-A62          TO  SF2-C46.
*  原単価（税抜）
*XXXXMOVE  HC2-A63          TO  SF2-C47.
     COMPUTE SF2-C47 =  HC2-A63  *  100.
*  原単価（税込）
*XXXXMOVE  HC2-A64          TO  SF2-C48.
     COMPUTE SF2-C48 =  HC2-A64  *  100.
*  税区分
     MOVE  HC2-A68          TO  SF2-C52.
*  税率
*XXXXMOVE  HC2-A69          TO  SF2-C53.
     COMPUTE SF2-C53 =  HC2-A69  *  10.
*  売単価（税抜）
     MOVE  HC2-A70          TO  SF2-C54.
*  売単価（税込）
     MOVE  HC2-A71          TO  SF2-C55.
*  センター経由区分
     MOVE  HC2-A94          TO  SF2-C62.
*  センターＣＤ
     MOVE  HC2-A95          TO  SF2-C63.
*#2020/03/26 NAV ST
     IF  PA-KANRNO  <  90000000
         MOVE  HC2-A90      TO  SF2-C59
         MOVE  HC2-A92      TO  SF2-C61
     ELSE
*********DISPLAY "SF2-C57 = " SF2-C57  UPON CONS
         EVALUATE  SF2-C57
             WHEN  "0"   MOVE  NC"ホーム"   TO  SF2-C59
                         MOVE  "ﾎｰﾑｾﾝﾀ"     TO  SF2-C61
             WHEN  "1"   MOVE  NC"２１ス"   TO  SF2-C59
                         MOVE  "21ｽﾀｲﾙ"     TO  SF2-C61
         END-EVALUATE
     END-IF.
*#2020/03/26 NAV ED
*  原価金額（税抜）
*XXXXCOMPUTE  SF2-C49  =  SF2-C44 * SF2-C47.
     COMPUTE  SF2-C49  =  ( SF2-C44 * HC2-A63 ) / 10.
*  原価金額（税込）
*XXXXCOMPUTE  SF2-C50  =  SF2-C44 * SF2-C48.
     COMPUTE  SF2-C50  =  ( SF2-C44 * HC2-A64 ) / 10.
*  税額
*XXXXCOMPUTE  SF2-C51  =  SF2-C49 * SF2-C53.
*T↓
*    DISPLAY "SF2-C49=" SF2-C49 UPON CONS.
*    DISPLAY "SF2-C53=" SF2-C53 UPON CONS.
*T↑
     COMPUTE  SF2-C51  =  ( SF2-C49 *  SF2-C53 ) / 1000.
*
     MOVE  "{"              TO  SF2-C75(28:1).
*
*レコード出力 ***********************************************
     WRITE  SF2-REC.
     ADD  1  TO  OT-NFASNSF2.
*
 EDWT-NFSUTDT-EXIT.
     EXIT.
****************************************************************
*  編集・出力／ナフコＡＳＮデータ（明細）ＦＡＸ発注用
****************************************************************
 EDWT-NFFAXDT-SEC             SECTION.
     MOVE  "EDWT-NFFAXDT-SEC" TO  S-NAME.

     MOVE  WSF2-REC         TO  SF2-REC.

*  伝票区分
     MOVE  02               TO  SF2-C11.
*  行番号
     MOVE  01               TO  SF2-C13.
*  入力区分
     MOVE  1                TO  SF2-C21.
*  総額取引区分
*XXXXMOVE  0                TO  SF2-C47.
     MOVE  0                TO  SF2-C46.
*  税区分
     MOVE  1                TO  SF2-C52.
*  税率
*XXXXCOMPUTE  SF2-C53  =  SKYTAXPG-OUT-DZEI / 100.
     COMPUTE  SF2-C53  =  SKYTAXPG-OUT-DZEI / 10.

*数量訂正ファイルより項目設定 *******************************
*  伝票番号
     MOVE  ST3-F09          TO  SF2-C12.
*  納品予定日
     MOVE  ST3-F08          TO  SF2-C16.
*  出荷日
     MOVE  ST3-F14          TO  SF2-C19.
*  発注形態区分
     MOVE  ST3-FIL(1:2)     TO  SF2-C20.
*  ＪＡＮコード
     MOVE  ST3-F10          TO  SF2-C31.
*  納品数量
     COMPUTE  SF2-C44  =  ST3-F11 * 10.
*  訂正理由区分
     IF  ST3-F12 = SPACE
         MOVE  ZERO         TO  SF2-C45
     ELSE
         MOVE  ST3-F12      TO  SF2-C45
     END-IF.

*ナフコ店舗マスから項目設定 *********************************
*  法人ＣＤ
     MOVE  WTEN-F21         TO  SF2-C06.
*  納品場所名漢字
     MOVE  WTEN-F22         TO  SF2-C59.
*  納品場所名カナ
     MOVE  WTEN-F23         TO  SF2-C61.


*基本情報ファイルから項目設定 *******************************
*  仕入先ＣＤ
     MOVE  KH1-HE13         TO  SF2-C09.
*  出荷業務仕入先ＣＤ
     MOVE  KH1-HE13         TO  SF2-C10.
*  発注日
     MOVE  "3"              TO  SKYDTCKB-IN-KBN.
     MOVE  KH1-HE11         TO  SKYDTCKB-IN-YMD6.
     MOVE  ZERO             TO  SKYDTCKB-IN-YMD8.
     MOVE  ZERO             TO  SKYDTCKB-OUT-RET.
     MOVE  ZERO             TO  SKYDTCKB-OUT-YMD.
     CALL  "SKYDTCKB"  USING SKYDTCKB-IN-KBN
                             SKYDTCKB-IN-YMD6
                             SKYDTCKB-IN-YMD8
                             SKYDTCKB-OUT-RET
                             SKYDTCKB-OUT-YMD.
     MOVE  SKYDTCKB-OUT-YMD TO  SF2-C15.
*#2020/05/26 NAV ST ＦＡＸの場合は発注日はシステム日付セット
     MOVE  SYS-DATE         TO  SF2-C15.
*#2020/05/26 NAV ED ＦＡＸの場合は発注日はシステム日付セット
*  出荷NO
     IF  PA-SYORI = 1
         MOVE  SF2-C06      TO  WK-SYUKANO-C06
         MOVE  SF2-C10      TO  WK-SYUKANO-C10
         MOVE  WK-SYUKANO   TO  SF2-C24
     ELSE
         MOVE  KH1-F42      TO  SF2-C24
     END-IF.
*#2020/03/26 NAV ST
     IF  PA-KANRNO  >=  90000000
*********DISPLAY "SF2-C57 = " SF2-C57  UPON CONS
         EVALUATE  SF2-C57
             WHEN  "0"   MOVE  NC"ホーム"   TO  SF2-C59
                         MOVE  "ﾎｰﾑｾﾝﾀ"     TO  SF2-C61
             WHEN  "1"   MOVE  NC"２１ス"   TO  SF2-C59
                         MOVE  "21ｽﾀｲﾙ"     TO  SF2-C61
         END-EVALUATE
     END-IF.
*#2020/03/26 NAV ED
*  梱包_
     MOVE  ALL "0"          TO  SF2-C26.
*  商品ＣＤ
     MOVE  KH1-F13          TO  SF2-C32.
*  発注数
     COMPUTE  SF2-C43  =  KH1-F19  * 10.
*  原単価（税抜）
     COMPUTE  SF2-C47  =  KH1-ME10 * 100.
*  原単価（税込）
     COMPUTE WK-KINGAKU = KH1-ME10 * 1.1 * 100.
     MOVE    WK-KINGAKU     TO  SF2-C48.
*  売単価（税抜）
     MOVE  KH1-ME12         TO  SF2-C54.
*  売単価（税込）
     COMPUTE WK-KINGAKU = KH1-ME12 * 1.1.
     MOVE    WK-KINGAKU     TO  SF2-C55.
*  原価金額（税抜）
*XXXXCOMPUTE  SF2-C49  =  SF2-C44 * SF2-C47.
     COMPUTE  SF2-C49  =  ( SF2-C44 / 10 ) * ( SF2-C47 / 100).
*  原価金額（税込）
*XXXXCOMPUTE  SF2-C50  =  SF2-C44 * SF2-C48.
     COMPUTE  SF2-C50  =  ( SF2-C44 *  SF2-C48 ) / 1000.
*  税額
*XXXXCOMPUTE  SF2-C51  =  SF2-C49 * SF2-C53.
     COMPUTE  SF2-C51  =  ( SF2-C49 *  SF2-C53 ) / 1000.

*ナフコ商品マスタから項目設定 *******************************
     MOVE  SF2-C32          TO  SYO-F01.
     PERFORM  RD-NFSHOMS-SEC.
     IF  FG-NFSHOMS-INV = 0
*  部門ＣＤ
         MOVE  SYO-F44      TO  SF2-C08
*  品名漢字
         MOVE  SYO-F05      TO  SF2-C36
*  品名カナ
         MOVE  SYO-F07      TO  SF2-C38
*  規格名漢字
         MOVE  SYO-F06      TO  SF2-C40
*  規格名カナ
         MOVE  SYO-F08      TO  SF2-C42
     ELSE
         MOVE  24           TO  SF2-C08
         MOVE  SPACE        TO  SF2-C36
                                SF2-C38
                                SF2-C40
                                SF2-C42
     END-IF.
*
     MOVE  "{"              TO  SF2-C75(28:1).
*
*レコード出力 ***********************************************
     WRITE  SF2-REC.
     ADD  1  TO  OT-NFASNSF2.
*
 EDWT-NFFAXDT-EXIT.
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
*    条件ファイル検索
****************************************************************
 RD-HJYOKEN-SEC             SECTION.
     MOVE  "RD-HJYOKEN-SEC" TO  S-NAME.
*
     READ  HJYOKEN
       INVALID
         MOVE  1                 TO  FG-HJYOKEN-INV
       NOT INVALID
         MOVE  ZERO              TO  FG-HJYOKEN-INV
     END-READ.
*
 RD-HJYOKEN-EXIT.
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
*  編集・出力／ナフコＡＳＮデータ（ヘッダ）
****************************************************************
 EDWT-NFASNSF1-SEC             SECTION.
     MOVE  "EDWT-NFASNSF1-SEC" TO  S-NAME.
*
*編集
     MOVE  SPACE            TO  SF1-REC.
     INITIALIZE                 SF1-REC.
     MOVE  "CH1"            TO  SF1-CH01.
     MOVE  SYS-DATE         TO  SF1-CH02(1:8).
     MOVE  WK-JYO-F04       TO  SF1-CH02(9:5).
     MOVE  SYS-DATE         TO  SF1-CH03.
     MOVE  SYS-DATE         TO  SF1-CH04.
     MOVE  SYS-TIME         TO  SF1-CH05.
     MOVE  137607           TO  SF1-CH06.
     MOVE  OT-NFASNSF2      TO  SF1-CH07.
     MOVE  0500             TO  SF1-CH08.
*
     MOVE  "{"              TO  SF1-CH09(444:1).
*
*レコード出力
     WRITE  SF1-REC.
     ADD  1  TO  OT-NFASNSF1.
*
 EDWT-NFASNSF1-EXIT.
     EXIT.
****************************************************************
*  終了処理                                          3.0       *
****************************************************************
 END-SEC                  SECTION.
     MOVE  "END-SEC"      TO  S-NAME.
*
     IF  OT-NFASNSF2  NOT =  ZERO
         PERFORM  EDWT-NFASNSF1-SEC
     END-IF.
*
*TEST<<<<<
*####     DISPLAY  "NFHAKOF  IN = " IN-NFHAKOF UPON CONS.
*####     DISPLAY  "NFHAKOF  SEL= " SL-NFHAKOF UPON CONS.
*####     DISPLAY  "NFSUTEF  IN = " IN-NFSUTEF UPON CONS.
*####     DISPLAY  "NFSUTEF  SEL= " SL-NFSUTEF UPON CONS.
*####     DISPLAY  "KEY-BREAK   = " OT-BREAK   UPON CONS.
*####     DISPLAY  "NFASNSF1 OT = "  OT-NFASNSF1  UPON CONS.
*TEST>>>>>
     DISPLAY  "NFASNSF2 OT = "  OT-NFASNSF2  UPON CONS.

* ファイルのＯＰＥＮ
     CLOSE  NFHAKOF.
     CLOSE  NFSUTEF.
     CLOSE  NFJOHOF.
     CLOSE  SAKUBAF.
     CLOSE  NFSHOMS.
     CLOSE  NFTENMS.
     CLOSE  NFHACPF.
     CLOSE  NFASNSF1.
     CLOSE  NFASNSF2.

 END-EXIT.
     EXIT.
*****************<<  SSY3891B   END PROGRAM  >>******************
