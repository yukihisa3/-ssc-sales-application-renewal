# SDE0060B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SDE0060B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　伝票ＥＸＣＥＬ連携　　　          *
*    モジュール名　　　　：　基本売上伝票データ作成　　　　　　*
*    作成日／作成者　　　：　2016/09/21 INOUE                  *
*    処理概要　　　　　　：　売上伝票ファイル作成のための　　　*
*                            基本データを作成する。　　　　　　*
*    更新日／更新者　　　：　2016/10/25 NAV TAKAHASHI          *
*    更新概要　　　　　　：　行番号＞６の場合、別伝票を起こす  *
*                            様に変更する。                    *
*    更新日／更新者　　　：　2017/01/16 NAV TAKAHASHI          *
*    更新概要　　　　　　：　ＷＫ備考１桁目＝９の時、９セット  *
*                          　以外は０セット　　　　　　　　　  *
*    更新日／更新者　　　：　2020/04/16 NAV TAKAHASHI          *
*    更新概要　　　　　　：　Ｄ３６５対応　　　　　　　　　　  *
*                          　Ｄ３６５番号の取得追加　　　　　  *
*    更新日／更新者　　　：　2020/12/18 NAV TAKAHASHI          *
*    更新概要　　　　　　：　Ｄ３６５対応　　　　　　　　　　  *
*                          　小売連携区分取得方法変更　　　　  *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SDE0060B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2016/09/21.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*伝票EXCEL取込ファイル
     SELECT   DEXXXXL2  ASSIGN    TO        DA-01-VI-DEXXXXL2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       DEX-F203  DEX-F204
                                            DEX-F207  DEX-F205
                                            DEX-F208  DEX-F206
                        FILE  STATUS   IS   DEX-STATUS.
*ナフコ商品マスタ
*    SELECT   NFSHOMS   ASSIGN    TO        DA-01-VI-NFSHOMS1
*                       ORGANIZATION        INDEXED
*                       ACCESS    MODE      RANDOM
*                       RECORD    KEY       NFM-F01
*                       FILE  STATUS   IS   NFM-STATUS.
*商品変換テーブル
     SELECT   HSHOTBL   ASSIGN    TO        DA-01-VI-SHOTBL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TBL-F01
                                            TBL-F02
                        FILE  STATUS   IS   TBL-STATUS.
*商品名称マスタ
     SELECT   HMEIMS    ASSIGN    TO        DA-01-VI-MEIMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       MEI-F01
                        FILE      STATUS    MEI-STATUS.
*取引先マスタ
     SELECT   HTOKMS    ASSIGN    TO        DA-01-VI-TOKMS2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TOK-F01
                        FILE      STATUS    TOK-STATUS.
*店舗マスタ
     SELECT   HTENMS    ASSIGN    TO        DA-01-VI-TENMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TEN-F52
                                            TEN-F011
                        FILE      STATUS    TEN-STATUS.
*#2020/12/18 NAV ST 小売連携区分（オーダー区分セット変更）
*ＳＵＢ商品変換ＴＢＬ
     SELECT   SUBTBLF   ASSIGN    TO        DA-01-VI-SUBTBLL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       SUB-F01
                                            SUB-F02
                        FILE      STATUS    SUB-STATUS.
*#2020/12/18 NAV ED 小売連携区分（オーダー区分セット変更）
*基本売上伝票データ
     SELECT   URIXXXF   ASSIGN    TO        DA-01-VS-URIXXXF
                        FILE      STATUS    DEN-STATUS.
*
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    伝票EXCEL取込ファイル
******************************************************************
 FD  DEXXXXL2            LABEL RECORD   IS   STANDARD.
     COPY     DEXXXXL2   OF        XFDLIB
              JOINING   DEX       PREFIX.
******************************************************************
*    ナフコ商品マスタ
******************************************************************
*FD  NFSHOMS            LABEL RECORD   IS   STANDARD.
*    COPY     NFSHOMS   OF        XFDLIB
*             JOINING   NFM       PREFIX.
******************************************************************
*    商品変換テーブル
******************************************************************
 FD  HSHOTBL            LABEL RECORD   IS   STANDARD.
     COPY     HSHOTBL   OF        XFDLIB
              JOINING   TBL       PREFIX.
******************************************************************
*    商品名称マスタ
******************************************************************
 FD  HMEIMS             LABEL RECORD   IS   STANDARD.
     COPY     HMEIMS    OF        XFDLIB
              JOINING   MEI       PREFIX.
******************************************************************
*    取引先マスタ
******************************************************************
 FD  HTOKMS             LABEL RECORD   IS   STANDARD.
     COPY     HTOKMS    OF        XFDLIB
              JOINING   TOK       PREFIX.
******************************************************************
*    店舗マスタ
******************************************************************
 FD  HTENMS             LABEL RECORD   IS   STANDARD.
     COPY     HTENMS    OF        XFDLIB
              JOINING   TEN       PREFIX.
*#2020/12/18 NAV ST
******************************************************************
*    ＳＵＢ商品変換ＴＢＬ
******************************************************************
 FD  SUBTBLF            LABEL RECORD   IS   STANDARD.
     COPY     SUBTBLF   OF        XFDLIB
              JOINING   SUB       PREFIX.
*#2020/12/18 NAV ED
******************************************************************
*    基本売上伝票データ
******************************************************************
 FD  URIXXXF            LABEL RECORD   IS   STANDARD.
     COPY     URIXXXF   OF        XFDLIB
              JOINING   DEN       PREFIX.
*
******************************************************************
 WORKING-STORAGE        SECTION.
******************************************************************
*FLG/ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  ZERO.
 01  DEX-FLG                 PIC  X(03)     VALUE  ZERO.
 01  DEL-FLG                 PIC  X(03)     VALUE  ZERO.
 01  DEN-ADD-CNT             PIC  9(07)     VALUE  ZERO.
 01  ERR-CNT                 PIC  9(07)     VALUE  ZERO.
 01  DEXXXXL2-READ-CNT       PIC  9(07)     VALUE  ZERO.
 01  HSHOTBL-INV-FLG         PIC  X(03)     VALUE  ZERO.
 01  HMEIMS-INV-FLG          PIC  X(03)     VALUE  ZERO.
 01  HTOKMS-INV-FLG          PIC  X(03)     VALUE  ZERO.
 01  HTENMS-INV-FLG          PIC  X(03)     VALUE  ZERO.
 01  NFSHOMS-INV-FLG         PIC  X(03)     VALUE  ZERO.
*#2020/12/18 NAV ST
 01  SUBTBLF-INV-FLG         PIC  X(03)     VALUE  ZERO.
*#2020/12/18 NAV ED
 01  GYO-CNT                 PIC  9(02)     VALUE  ZERO.
 01  INIT-FLG                PIC  X(01)     VALUE  "1".
*#2017/03/14 NAV ST
 01  DPNO-SET-FLG            PIC  X(03)     VALUE  SPACE.
 01  DPNO-TOKCD              PIC  9(08)     VALUE  ZERO.
*#2017/03/14 NAV ED
*
 01  TOK-INV-FLG             PIC  X(03)     VALUE  SPACE.
*
 01  WRK-AREA.
     03  OUT-DENNO           PIC  9(09)     VALUE  ZERO.
 01  LINK-AREA-SUB1.
     03  LI-TRCD             PIC  9(08).
     03  LO-ERR              PIC  9(01).
     03  LO-DENNO            PIC  9(09).
     03  LO-NEXT             PIC  9(09).
*
*01  WRK-AREA.
*    03  WRK-TEISUU          PIC S9(09)V99  VALUE  ZERO.
*    03  WRK-MAETEISUU       PIC S9(09)V99  VALUE  ZERO.
*行0保管ワーク
     COPY     DEXXXXL2   OF        XFDLIB
              JOINING    SAV       PREFIX.
*ブレイクキー
 01  BRK-AREA.
     03  BRK-F203          PIC X(01)         VALUE SPACE.
     03  BRK-F204          PIC 9(08)         VALUE ZERO.
     03  BRK-F207          PIC X(02)         VALUE SPACE.
     03  BRK-F205          PIC 9(09)         VALUE ZERO.
     03  BRK-F208          PIC 9(05)         VALUE ZERO.
     03  BRK-F206          PIC 9(02)         VALUE ZERO.
*計算領域
 01  WRK-AREA2.
     03  WRK-HIK           PIC S9(09)V9(02)  VALUE ZERO.
     03  WRK-ZAI           PIC S9(09)V9(02)  VALUE ZERO.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
*システム時刻の編集
     03  SYSTIME           PIC  9(08)  VALUE  ZERO.
     03  SYSTIMER          REDEFINES   SYSTIME.
         05  SYS-TIME1     PIC  9(06).
         05  SYS-TIME2     PIC  9(02).
 01  WK-ST.
     03  DEX-STATUS        PIC  X(02).
     03  NFM-STATUS        PIC  X(02).
     03  TBL-STATUS        PIC  X(02).
     03  MEI-STATUS        PIC  X(02).
     03  TOK-STATUS        PIC  X(02).
     03  TEN-STATUS        PIC  X(02).
     03  DEN-STATUS        PIC  X(02).
*#2020/12/16 NAV ST
     03  SUB-STATUS        PIC  X(02).
*#2020/12/16 NAV ED
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SDE0060B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SDE0060B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SDE0060B".
         05  FILLER         PIC   X(11)  VALUE
                                         " ABEND *** ".
     03  ABEND-FILE.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  AB-FILE        PIC   X(08).
         05  FILLER         PIC   X(06)  VALUE " ST = ".
         05  AB-STS         PIC   X(02).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  SEC-NAME.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(07)  VALUE " SEC = ".
         05  S-NAME         PIC   X(30).
*日付サブルーチン用
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*#2020/04/16 NAV ST Ｄ３６５伝票番号取得
 01  WK-D365-DEN-PARA.
     03  WK-D365-PARA-IN1   PIC   X(01).
     03  WK-D365-PARA-IN2   PIC   9(08).
     03  WK-D365-PARA-OUT1  PIC   X(20).
     03  WK-D365-PARA-OUT2  PIC   X(01).
*#2020/04/16 NAV ED Ｄ３６５伝票番号取得
*
 LINKAGE                SECTION.
 01  PARA-IN-TANCD      PIC   X(02).
 01  PARA-IN-BUMCD      PIC   X(04).
 01  PARA-OUT-DATE      PIC   9(08).
 01  PARA-OUT-TIME      PIC   9(06).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-IN-TANCD
                                       PARA-IN-BUMCD
                                       PARA-OUT-DATE
                                       PARA-OUT-TIME.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   DEXXXXL2.
     MOVE      "DEXXXXL2"   TO   AB-FILE.
     MOVE      DEX-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
*FILEERR-SEC2           SECTION.
*    USE       AFTER    EXCEPTION
*                       PROCEDURE   NFSHOMS.
*    MOVE      "NFSHOMS1"   TO   AB-FILE.
*    MOVE      NFM-STATUS   TO   AB-STS.
*    DISPLAY   MSG-ABEND         UPON CONS.
*    DISPLAY   SEC-NAME          UPON CONS.
*    DISPLAY   ABEND-FILE        UPON CONS.
*    MOVE      4000         TO   PROGRAM-STATUS.
*    STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HSHOTBL.
     MOVE      "SHOTBL1 "   TO   AB-FILE.
     MOVE      TBL-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC4           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HMEIMS.
     MOVE      "MEIMS1  "   TO   AB-FILE.
     MOVE      MEI-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC5           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HTOKMS.
     MOVE      "TOKMS1  "   TO   AB-FILE.
     MOVE      TOK-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC6           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HTENMS.
     MOVE      "TENMS1  "   TO   AB-FILE.
     MOVE      TEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC7           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   URIXXXF.
     MOVE      "URIXXXF"   TO   AB-FILE.
     MOVE      DEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
*
 FILEERR-SEC8           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SUBTBLF.
     MOVE      "SUBTBLL1"  TO   AB-FILE.
     MOVE      SUB-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 END     DECLARATIVES.
*****************************************************************
*                                                                *
******************************************************************
 GENERAL-PROCESS       SECTION.
*
     MOVE     "PROCESS-START"     TO   S-NAME.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC   UNTIL  END-FLG = "END".
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
     INITIALIZE                   WRK-AREA.
     OPEN     INPUT     DEXXXXL2  HSHOTBL  HMEIMS
                        HTOKMS    HTENMS
*#2020/12/18 NAV ST
                        SUBTBLF
*#2020/12/18 NAV ED
     OPEN     OUTPUT    URIXXXF.
*
     DISPLAY  MSG-START UPON CONS.
*
******************
*システム日付編集*
******************
     ACCEPT      SYS-DATE  FROM      DATE.
     MOVE       "3"        TO        LINK-IN-KBN.
     MOVE        SYS-DATE  TO        LINK-IN-YMD6.
     CALL       "SKYDTCKB"   USING   LINK-IN-KBN
                                     LINK-IN-YMD6
                                     LINK-IN-YMD8
                                     LINK-OUT-RET
                                     LINK-OUT-YMD8.
     IF          LINK-OUT-RET   =    ZERO
         MOVE    LINK-OUT-YMD8  TO   SYS-DATEW
     ELSE
         MOVE    ZERO           TO   SYS-DATEW
     END-IF.
     ACCEPT      SYSTIME   FROM      TIME.
     MOVE        SYS-DATEW TO        PARA-OUT-DATE.
     MOVE        SYS-TIME1 TO        PARA-OUT-TIME.
*伝票EXCEL取込ファイル対象レコード有無判定
     PERFORM  DEXXXXL2-START-SEC.
*ワーク行カウントリセット
     MOVE  1                     TO      GYO-CNT.
*伝票番号採番
     IF    DEX-F203  =  " "
           PERFORM   DPNO-SET-SEC
*↓TEST
*****DISPLAY "INIT-SEC DENNO=" OUT-DENNO UPON CONS
*↑TEST
*#2017/03/14 NAV ST
           MOVE      "SET"       TO   DPNO-SET-FLG
           MOVE      DEX-F204    TO   DPNO-TOKCD
           DISPLAY NC"伝票_採番取引先" " = " DPNO-TOKCD
                   UPON CONS
*#2017/03/14 NAV ED
*#2020/04/16 NAV ST
           PERFORM   D365-DEN-SEC
*#2020/04/E6 NAV ED
*#2020/04/16 NAV ST
     ELSE
           PERFORM   D365-DEN-SEC
*#2020/04/E6 NAV ED
     END-IF.
*
 INIT-EXIT.
     EXIT.
*
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"          TO   S-NAME.
*
*レコード初期化
     MOVE     SPACE              TO   DEN-REC.
     INITIALIZE                       DEN-REC.
*
*ブレイク判定
     IF  ( DEX-F203 NOT = BRK-F203 )  OR
         ( DEX-F204 NOT = BRK-F204 )  OR
         ( DEX-F207 NOT = BRK-F207 )  OR
         ( DEX-F205 NOT = BRK-F205 )  OR
         ( DEX-F208 NOT = BRK-F208 )  OR
*#2016/10/25 NAV ST 行番号＞６のチェック
         ( GYO-CNT  >     6        )
*#2016/10/25 NAV ED 行番号＞６のチェック
*    -------------------------
*    - ブレイクＲＥＣ_行＝0 -
*    -------------------------
           IF    DEX-F206     =        0
*               行0→行80レコード作成
                 PERFORM      GYO80-WRITE-SEC
*               レコード初期化
                 MOVE    SPACE              TO  DEN-REC
                 INITIALIZE                     DEN-REC
*
*               行0レコード保管
                 MOVE  DEX-REC         TO      SAV-REC
*               ワーク行カウントリセット
                 MOVE  1               TO      GYO-CNT
*
                 IF  DEX-F202  =  "3"    *>伝票番号採番
*                   伝票番号採番
                     PERFORM   DPNO-SET-SEC
*********************#2020/04/16 NAV ST
                     PERFORM   D365-DEN-SEC
*********************#2020/04/16 NAV ED
*↓TEST
*********************DISPLAY "MAIN-1 DENNO=" OUT-DENNO UPON CONS
*↑TEST
*                  ブレイクキー入替
*                   制御区分
                     MOVE  DEX-F203        TO      BRK-F203
*                   取引先ＣＤ
                     MOVE  DEX-F204        TO      BRK-F204
*                   伝票区分
                     MOVE  DEX-F207        TO      BRK-F207
*                   伝票番号
                     MOVE  DEX-F205        TO      BRK-F205
*                   店舗ＣＤ
                     MOVE  DEX-F208        TO      BRK-F208
*                   行
                     MOVE  DEX-F206        TO      BRK-F206
                 END-IF
*               次ＲＥＡＤへ
                 GO                    TO      MAIN-010
*    -------------------------
*    - ブレイクＲＥＣ_行≠0 -
*    -------------------------
           ELSE
             IF  DEX-F202  =  "1" OR "2"   *>伝票NO指定、採番
*              ブレイクキー入替
*               制御区分
                 MOVE  DEX-F203        TO      BRK-F203
*               取引先ＣＤ
                 MOVE  DEX-F204        TO      BRK-F204
*               伝票区分
                 MOVE  DEX-F207        TO      BRK-F207
*               伝票番号
                 MOVE  DEX-F205        TO      BRK-F205
*               店舗ＣＤ
                 MOVE  DEX-F208        TO      BRK-F208
*               行
                 MOVE  DEX-F206        TO      BRK-F206
*               ワーク行カウントリセット
                 MOVE  1               TO      GYO-CNT
*               伝票番号採番
                 IF    DEX-F203  =  " "
                       PERFORM   DPNO-SET-SEC
*↓TEST
*****DISPLAY "MAIN-2 DENNO=" OUT-DENNO UPON CONS
*↑TEST
***********************#2020/04/16 NAV ST
                       PERFORM   D365-DEN-SEC
***********************#2020/04/16 NAV ED
                 ELSE
***********************#2020/04/16 NAV ST
                       PERFORM   D365-DEN-SEC
***********************#2020/04/16 NAV ED
                 END-IF
             ELSE
              IF BRK-F206 NOT = 0  OR  INIT-FLG = "1"
*               行0→行80レコード作成
                 PERFORM      GYO80-WRITE-SEC
*               レコード初期化
                 MOVE    SPACE              TO  DEN-REC
                 INITIALIZE                     DEN-REC
                 MOVE    SPACE              TO  INIT-FLG
              END-IF
*               伝票番号採番
                 IF  ( DEX-F203      =  " " ) AND
                     ( BRK-F208  NOT = ZERO )
                       PERFORM   DPNO-SET-SEC
*↓TEST
***********************DISPLAY "MAIN-3 DENNO=" OUT-DENNO UPON CONS
*↑TEST
***********************#2020/04/16 NAV ST
                       PERFORM   D365-DEN-SEC
***********************#2020/04/16 NAV ED
                 END-IF
*               ブレイクキー入替
*                制御区分
                 MOVE  DEX-F203        TO      BRK-F203
*                取引先ＣＤ
                 MOVE  DEX-F204        TO      BRK-F204
*                伝票区分
                 MOVE  DEX-F207        TO      BRK-F207
*                伝票番号
                 MOVE  DEX-F205        TO      BRK-F205
*                店舗ＣＤ
                 MOVE  DEX-F208        TO      BRK-F208
*                行
                 MOVE  DEX-F206        TO      BRK-F206
*               ワーク行カウントリセット
                 MOVE  1               TO      GYO-CNT
             END-IF
           END-IF
     END-IF.
*
*
*-------------------------
*明細レコード作成
*-------------------------
*
*マスタを索引する。
     PERFORM HSHOTBL-READ-SEC.
*#2020/12/18 NAV ST
     PERFORM SUBTBLF-READ-SEC.
*#2020/12/18 NAV ED
     IF  HSHOTBL-INV-FLG = SPACE
         PERFORM HMEIMS-READ-SEC
     END-IF.
     PERFORM HTOKMS-READ-SEC.
     PERFORM HTENMS-READ-SEC.
*全マスタを索引し、１つでも存在しない場合は取り込まない。
     IF  HSHOTBL-INV-FLG = "INV"
         DISPLAY "変換ＴＢＬなし　　"                 UPON CONS
         DISPLAY "　取引先ＣＤ　　＝" DEX-F204        UPON CONS
         DISPLAY "　相手商品ＣＤ　＝" DEX-F216        UPON CONS
         ADD    1               TO  ERR-CNT
         GO                     TO  MAIN-010
     END-IF.
     IF  HMEIMS-INV-FLG  = "INV"
         DISPLAY "名称マスタなし　　"                 UPON CONS
         DISPLAY "　自社商品ＣＤ　＝" TBL-F031        UPON CONS
         DISPLAY "　自社品単ＣＤ　＝" TBL-F032        UPON CONS
         ADD    1               TO  ERR-CNT
         GO                     TO  MAIN-010
     END-IF.
     IF  HTOKMS-INV-FLG  = "INV"
         DISPLAY "取引先マスタなし　"                 UPON CONS
         DISPLAY "　取引先ＣＤ　　＝" DEX-F204        UPON CONS
         ADD    1               TO  ERR-CNT
         GO                     TO  MAIN-010
     END-IF.
     IF  HTENMS-INV-FLG  = "INV"
         DISPLAY "店舗マスタなし　　"                 UPON CONS
         DISPLAY "　取引先ＣＤ　　＝" DEX-F204        UPON CONS
         DISPLAY "　店舗ＣＤ　　　＝" DEX-F208        UPON CONS
         ADD    1               TO  ERR-CNT
         GO                     TO  MAIN-010
     END-IF.
*
*項目セット
*  取引先コード
     MOVE    DEX-F204           TO  DEN-F01.
*  自社伝票番号
     IF      DEX-F203  =  "D"
             MOVE    DEX-F205   TO  DEN-F02
     ELSE
             MOVE    OUT-DENNO  TO  DEN-F02
     END-IF.
*  行番号
     MOVE    GYO-CNT            TO  DEN-F03.
     ADD     1                  TO  GYO-CNT.
*  相殺区分
     MOVE    ZERO               TO  DEN-F04.
*  伝区コード
     MOVE    DEX-F207           TO  DEN-F051.
*  伝区名称
     EVALUATE  DEX-F207
       WHEN  "40"
             MOVE  NC"売上伝票" TO  DEN-F052
       WHEN  "41"
             MOVE  NC"返品伝票" TO  DEN-F052
       WHEN  "42"
             MOVE  NC"値引伝票" TO  DEN-F052
       WHEN  OTHER
             MOVE  NC"？？？？" TO  DEN-F052
     END-EVALUATE.
*  担当者
     MOVE    PARA-IN-TANCD      TO  DEN-F06.
*  店舗コード
     MOVE    DEX-F208           TO  DEN-F07.
*  出荷場所
     MOVE    DEX-F209           TO  DEN-F08.
*  伝発場所
     MOVE    DEX-F210           TO  DEN-F09.
*  注文番号  F10
*  注文日
     MOVE    DEX-F211           TO  DEN-F111.
*  納品日
     MOVE    DEX-F212           TO  DEN-F112.
*  出荷日
*  分類
     MOVE    DEX-F213           TO  DEN-F12.
*  区分:商区
     MOVE    DEX-F214           TO  DEN-F131.
*  区分:伝票
     MOVE    DEX-F215           TO  DEN-F132.
*  区分:請求 F133
*  区分:伝発 F134
*#2017/01/16 NAV ST
*****MOVE    ZERO               TO  DEN-F134.
*****DISPLAY "SAV-F222 1 = " SAV-F222 UPON CONS.
     IF  SAV-F222(1:1)  =  "9"
         MOVE "9"               TO  DEN-F134
     ELSE
         MOVE "0"               TO  DEN-F134
     END-IF.
*#2017/01/15 NAV ED
*  自社商品コード
     MOVE    DEX-F223           TO  DEN-F1411.
*  自社品単コード
     MOVE    DEX-F224           TO  DEN-F1412(1:5).
     MOVE    DEX-F225           TO  DEN-F1412(6:2).
     MOVE    DEX-F226           TO  DEN-F1412(8:1).
*  商品名１
     MOVE    DEX-F217           TO  DEN-F1421.
*  商品名２
     MOVE    DEX-F218           TO  DEN-F1422.
*  数量
     MOVE    DEX-F219           TO  DEN-F15.
*  単
     MOVE    "1"                TO  DEN-F16.
*  仕入単価
     MOVE    TBL-F09            TO  DEN-F171.
*  原価単価
     MOVE    DEX-F220           TO  DEN-F172.
*  売価単価
     MOVE    DEX-F221           TO  DEN-F173.
*        原価／売価が０の場合は、商品変換ＴＢＬの単価をセット
*        IF  DEX-F220 = ZERO
*            MOVE  TBL-F05      TO  DEN-F172
*        END-IF.
*        IF  DEX-F221 = ZERO
*            MOVE  TBL-F06      TO  DEN-F173
*        END-IF.
*  原価金額
     COMPUTE DEN-F181 = DEN-F15 * DEN-F172.
*  売価金額
     COMPUTE DEN-F182 = DEN-F15 * DEN-F173.
*  消費税 F19
*  粗利　 F20
*  ストック番号　 F21
*  備考  行≠0の場合のみ
     IF      DEX-F206  NOT =    ZERO
             MOVE   DEX-F222    TO  DEN-F22
     ELSE
             MOVE   SPACE       TO  DEN-F22
     END-IF.
*  指定伝票番号
*    MOVE    DEX-F205           TO  DEN-F23.
     IF      DEX-F203  =  "D"
             MOVE    DEX-F205   TO  DEN-F23
     ELSE
             MOVE    OUT-DENNO  TO  DEN-F23
     END-IF.
*  自社得意先コード
     MOVE    DEX-F227           TO  DEN-F24.
*  指定商品コード
     MOVE    DEX-F216           TO  DEN-F25.
*  フラグ１  F261
*  フラグ２  F262
*  フラグ３  F263
*  フラグ４  F264
*  フラグ５  F265
*  区分フラグ:チェックリスト F271
*  区分フラグ:伝票発行
     MOVE    9                  TO  DEN-F272.
*  区分フラグ:修正 F273
*  区分フラグ:オンライン
     MOVE    0                  TO  DEN-F274.
*  区分フラグ:エントリー
     MOVE    1                  TO  DEN-F275.
*  区分フラグ:付番
     MOVE    TOK-F89            TO  DEN-F276.
*  区分フラグ:売上データ作成 F277
*  区分フラグ:量販店
     MOVE    "A"                TO  DEN-F278.
*  区分フラグ:端数金額
*  区分フラグ:端数消費税
*  区分フラグ:付番修正
     MOVE    TOK-F89            TO  DEN-F27B.
*  区分フラグ:予備２ F27C
*  区分フラグ:予備３ F27D
*  区分フラグ:予備４ F27E
*  区分フラグ:予備５ F27F
*  WS番号 F28
*  変換値 F29
*  店舗名カナ
     MOVE    TEN-F04            TO  DEN-F30.
*  伝票枚数  F98
*  システム日付
     MOVE    SYS-DATEW          TO  DEN-F99.
*  オンライン伝票枚数 F40
*  欠品制御区分       F31
*  小売連携対象区分   F32
*#2020.12.18 NAV ST
*****MOVE    MEI-F10            TO  DEN-F32.
     IF  SUBTBLF-INV-FLG = "INV"
         MOVE SPACE             TO  DEN-F32
     ELSE
         MOVE SUB-F19           TO  DEN-F32
     END-IF.
*#2020.12.18 NAV ED
*  小売連携_         F33
*  税区分             F34
*  税抜計算済FLG      F35
*  支払抽出区分       F411
*  支払計上区分       F412
*  支払計上日         F413
*  ルート             F42
*  ストック番号新     F43
*  注文番号（新）     F44
*  売上計上日付 年    F451
*  売上計上日付 月    F452
*  売上計上日付 日    F453
*  受信日付 時        F471
*  受信日付 分        F472
*  振分倉庫コード
     MOVE    DEX-F209           TO  DEN-F48.
*  棚番１
     MOVE    TBL-F081           TO  DEN-F491.
*  棚番２
     MOVE    TBL-F082           TO  DEN-F492.
*  棚番３
     MOVE    TBL-F083           TO  DEN-F493.
*  訂正前数量
     MOVE    DEX-F219           TO  DEN-F50.
*  訂正前単価 仕入単価
     MOVE    TBL-F09            TO  DEN-F511.
*  訂正前単価 原価単価
     MOVE    DEN-F172           TO  DEN-F512.
*  訂正前単価 売価単価
     MOVE    DEN-F173           TO  DEN-F513.
*  訂正前金額 原価単価 (数量×原価単価)
     COMPUTE DEN-F521           =   DEX-F219  *  DEN-F172
*  訂正前金額 売価単価 (数量×売価単価)
     COMPUTE DEN-F522           =   DEX-F219  *  DEN-F173
*  訂正フラグ    F53
*  更新済フラグ  F54
*  エラーFLG     F55
*  出荷完了FLG   F56
*  元出荷場所    F57
*  欠品区分      F58
*  登録担当者
     MOVE    PARA-IN-TANCD      TO  DEN-F59.
*  更新担当者    F60
*  承認担当者    F61
*  登録日付
     MOVE    SYS-DATEW          TO  DEN-F62.
*  更新日付      F63
*  承認日付      F64
*  代表権限      F65
*  権限１        F66
*  最終更新時刻
     MOVE    SYS-TIME1          TO  DEN-F67.
*  物流連携ＦＬＧ F68
*  物流連携日     F69
*  伝票番号採番区分
     IF      DEX-F203   =   "D"
             MOVE       "1"     TO  DEN-F70
     ELSE
             MOVE       " "     TO  DEN-F70
     END-IF.
*Ｄ３６５伝票番号セット
*#2020/04/16 NAV ST
     MOVE    WK-D365-PARA-OUT1  TO  DEN-D99.
*#2020/04/16 NAV ST.
*
     WRITE   DEN-REC.
     ADD     1                  TO  DEN-ADD-CNT.
*
 MAIN-010.
     PERFORM  DEXXXXL2-READ-SEC.
*
     IF  DEX-FLG  = "END"
         MOVE       "END"        TO   END-FLG
*       レコード初期化
         MOVE        SPACE       TO   DEN-REC
         INITIALIZE                   DEN-REC
*       行0→行80レコード作成
         PERFORM     GYO80-WRITE-SEC
     END-IF.
*
 MAIN-EXIT.
     EXIT.
*
****************************************************************
*　　行＝80レコード作成
****************************************************************
 GYO80-WRITE-SEC     SECTION.
*
     MOVE    "GYO80-WRITE-SEC"    TO   S-NAME.
*
*        取引先コード
           MOVE    SAV-F204           TO  DEN-F01.
*        自社伝票番号
*          MOVE    SAV-F205           TO  DEN-F02.
           IF      SAV-F203  =  "D"
                   MOVE    SAV-F205   TO  DEN-F02
           ELSE
                   MOVE    OUT-DENNO  TO  DEN-F02
           END-IF.
*        行番号
           MOVE    80                 TO  DEN-F03.
*        相殺区分
           MOVE    ZERO               TO  DEN-F04.
*        伝区コード
           MOVE    SAV-F207           TO  DEN-F051.
*        伝区名称
           EVALUATE  SAV-F207
             WHEN  "40"
                   MOVE  NC"売上伝票" TO  DEN-F052
             WHEN  "41"
                   MOVE  NC"返品伝票" TO  DEN-F052
             WHEN  "42"
                   MOVE  NC"値引伝票" TO  DEN-F052
             WHEN  OTHER
                   MOVE  NC"？？？？" TO  DEN-F052
           END-EVALUATE.
*        担当者
           MOVE    PARA-IN-TANCD      TO  DEN-F06.
*        店舗コード
           IF      DEX-F202 = "1" OR "2"
                   MOVE    SAV-F208   TO  DEN-F07
           ELSE
                   MOVE    BRK-F208   TO  DEN-F07
           END-IF.
*        出荷場所
           MOVE    SAV-F209           TO  DEN-F08.
*        伝発場所
           MOVE    SAV-F210           TO  DEN-F09.
*        注文番号F10
*        注文日
           MOVE    SAV-F211           TO  DEN-F111.
*        納品日
           MOVE    SAV-F212           TO  DEN-F112.
*        出荷日
*        分類
           MOVE    SAV-F213           TO  DEN-F12.
*        区分:商区
           MOVE    SAV-F214           TO  DEN-F131.
*        区分:伝票
           MOVE    SAV-F215           TO  DEN-F132.
*        区分:請求F133
*        区分:伝発F134
*********#2017/01/16 NAV ST
***********MOVE    ZERO               TO  DEN-F134.
*****DISPLAY "SAV-F222 2 = " SAV-F222 UPON CONS.
           IF  SAV-F222(1:1)  =  "9"
               MOVE "9"               TO  DEN-F134
           ELSE
               MOVE "0"               TO  DEN-F134
           END-IF.
*********#2017/01/16 NAV ED
*        自社商品コードF1411
*        自社品単コードF1412
*        商品名１
           MOVE    SAV-F217           TO  DEN-F1421.
*        商品名２
           MOVE    SAV-F218           TO  DEN-F1422.
*        数量F15
*        単
           MOVE    "1"                TO  DEN-F16.
*        仕入単価F171
*        原価単価F172
*        売価単価F173
*        原価金額F181
*        売価金額F182
*        消費税F19
*        粗利F20
*        ｽﾄｯｸ番号F21
*        備考F22
*        指定伝票番号
*          MOVE    SAV-F205           TO  DEN-F23.
           IF      SAV-F203  =  "D"
                   MOVE    SAV-F205   TO  DEN-F23
           ELSE
                   MOVE    OUT-DENNO  TO  DEN-F23
           END-IF.
*        自社得意先コード
           MOVE    SAV-F227           TO  DEN-F24.
*        指定商品コードF25
*        フラグ１F261
*        フラグ２F262
*        フラグ３F263
*        フラグ４F264
*        フラグ５F265
*        区分フラグ:チェックリストF271
*        区分フラグ:伝票発行
           MOVE    9                  TO  DEN-F272.
*        区分フラグ:修正 F273
*        区分フラグ:オンライン
***********MOVE    1                  TO  DEN-F274.
*********#2017/01/15 NAV ST
           MOVE    0                  TO  DEN-F274.
*********#2017/01/15 NAV ED
*        区分フラグ:エントリー
           MOVE    1                  TO  DEN-F275.
*        区分フラグ:付番
           MOVE    TOK-F89            TO  DEN-F276.
*        区分フラグ:売上データ作成 F277
*        区分フラグ:量販店
           MOVE    "A"                TO  DEN-F278.
*        区分フラグ:端数金額F279
*        区分フラグ:端数消費税F27A
*        区分フラグ:付番修正
           MOVE    TOK-F89            TO  DEN-F27B.
*        区分フラグ:予備２F27C
*        区分フラグ:予備３F27D
*        区分フラグ:予備４F27E
*        区分フラグ:予備５F27F
*        WS番号F28
*        変換値F29
*        店舗名カナF30
*        伝票枚数F98
*        システム日付
           MOVE    SYS-DATEW          TO  DEN-F99.
*        オンライン伝票枚数F40
*        欠品制御区分 F31
*        小売連携対象区分 F32
*        小売連携_ F33
*        税区分 F34
*        税抜計算済FLG F35
*        支払抽出区分 F411
*        支払計上区分 F412
*        支払計上日 F413
*        ルート F42
*        ストック番号新 F43
*        注文番号（新） F44
*        売上計上日付 年 F451
*        売上計上日付 月 F452
*        売上計上日付 日 F453
*        受信日付 時 F471
*        受信日付 分 F472
*        振分倉庫コード
           MOVE    SAV-F209           TO  DEN-F48.
*        棚番１ F491
*        棚番２ F492
*        棚番３ F493
*        訂正前数量 F50
*        訂正前単価 仕入単価 F511
*        訂正前単価 原価単価 F512
*        訂正前単価 売価単価 F513
*        訂正前金額 原価単価 (数量×原価単価) F521
*        訂正前金額 売価単価 (数量×売価単価) F522
*        訂正フラグ F53
*        更新済フラグ F54
*        エラーFLG F55
*        出荷完了FLG F56
*        元出荷場所 F57
*        欠品区分 F58
*        登録担当者
           MOVE    PARA-IN-TANCD      TO  DEN-F59.
*        更新担当者 F60
*        承認担当者 F61
*        登録日付
           MOVE    SYS-DATEW          TO  DEN-F62.
*        更新日付 F63
*        承認日付 F64
*        代表権限 F65
*        権限１ F66
*        最終更新時刻
           MOVE    SYS-TIME1          TO  DEN-F67.
*        物流連携ＦＬＧ F68
*        物流連携日 F69
*        伝票番号採番区分
           IF      SAV-F203   =   "D"
                   MOVE       "1"     TO  DEN-F70
           ELSE
                   MOVE       " "     TO  DEN-F70
           END-IF.
*
*        レコードＷＲＩＴＥ
           WRITE   DEN-REC.
           ADD     1                  TO  DEN-ADD-CNT.
*
 GYO80-WRITE-EXIT.
     EXIT.
*
****************************************************************
*　　伝票EXCEL取込ファイル　スタート
****************************************************************
 DEXXXXL2-START-SEC     SECTION.
*
     MOVE    "DEXXXXL2-READ-SEC"  TO   S-NAME.
*
     MOVE     SPACE              TO   DEX-REC.
     INITIALIZE                       DEX-REC.
*
     MOVE     SPACE              TO   DEX-F203.
     MOVE     ZERO               TO   DEX-F204.
     MOVE     SPACE              TO   DEX-F207.
     MOVE     ZERO               TO   DEX-F205.
     MOVE     ZERO               TO   DEX-F208.
     MOVE     ZERO               TO   DEX-F206.
*
     START  DEXXXXL2  KEY  IS  >= DEX-F203 DEX-F204 DEX-F207
                                  DEX-F205 DEX-F208 DEX-F206
           INVALID
           MOVE  "END"           TO      END-FLG
           DISPLAY "＃処理対象がありません！＃" UPON CONS
           GO                    TO      DEXXXXL2-START-EXIT
     END-START.
*伝票EXCEL取込ファイル読込(1件目　行＝0)
     PERFORM  DEXXXXL2-READ-SEC.
     IF    END-FLG = "END"
           DISPLAY "＃処理対象がありません！＃" UPON CONS
           GO                    TO      DEXXXXL2-START-EXIT
     END-IF.
*１件目の行_＝0以外は終了
     IF    DEX-F206 NOT = 0
           DISPLAY "＃１件目の行が０以外！　＃" UPON CONS
           MOVE    "END"         TO      END-FLG
           GO                    TO      DEXXXXL2-START-EXIT
     END-IF.
*
*行0レコード保管
     MOVE  DEX-REC               TO      SAV-REC.
*↓TEST
*    DISPLAY "SAV-F203=" SAV-F203 UPON CONS
*    DISPLAY "SAV-F204=" SAV-F204 UPON CONS
*    DISPLAY "SAV-F207=" SAV-F207 UPON CONS
*    DISPLAY "SAV-F205=" SAV-F205 UPON CONS
*    DISPLAY "SAV-F208=" SAV-F208 UPON CONS
*    DISPLAY "SAV-F206=" SAV-F206 UPON CONS
*↑TEST
*---------------------------------------------------------------
*伝票EXCEL取込ファイル読込(2件目　行≠0)
     PERFORM  DEXXXXL2-READ-SEC.
     IF    END-FLG = "END"
           DISPLAY "＃処理対象がありません！＃" UPON CONS
           GO                    TO      DEXXXXL2-START-EXIT
     END-IF.
*１件目の行_＝0は終了
     IF    DEX-F206 = 0
           DISPLAY "＃２件目の行が０　！？　＃" UPON CONS
           MOVE    "END"         TO      END-FLG
           GO                    TO      DEXXXXL2-START-EXIT
     END-IF.
*
*ブレイクキー入替
*  制御区分
     MOVE  DEX-F203              TO      BRK-F203.
*  取引先ＣＤ
     MOVE  DEX-F204              TO      BRK-F204.
*  伝票区分
     MOVE  DEX-F207              TO      BRK-F207.
*  伝票番号
     MOVE  DEX-F205              TO      BRK-F205.
*  店舗ＣＤ
     MOVE  DEX-F208              TO      BRK-F208.
*
 DEXXXXL2-START-EXIT.
     EXIT.
****************************************************************
*　　伝票EXCEL取込ファイル READ
****************************************************************
 DEXXXXL2-READ-SEC            SECTION.
*
     MOVE    "DEXXXXL2-READ-SEC" TO        S-NAME.
*
     READ  DEXXXXL2  AT  END
           MOVE  "END"          TO        DEX-FLG
           GO                   TO        DEXXXXL2-READ-EXIT
     END-READ.
*件数カウント
     ADD      1                   TO   DEXXXXL2-READ-CNT.
*↓TEST
*    DISPLAY "READ DENNO=" DEX-F02 UPON CONS.
*    DISPLAY "     GYONO=" DEX-F03 UPON CONS.
*↑TEST
 READ010.
*更新区分チェック
*    IF       DEX-F10  =  "1"
*             MOVE     "END"      TO   END-FLG
*             GO                  TO   DEXXXXL2-READ-EXIT
*    END-IF.
 READ020.
*エラー区分チェック
     IF       DEX-F301 =  "1"
              DISPLAY "＃データにエラーあり！？＃" UPON CONS
              MOVE     "END"      TO   END-FLG
              GO                  TO   DEXXXXL2-READ-EXIT
     END-IF.
 READ030.
*オンライン／手書区分チェック
*    IF       DEX-F01  NOT =  "2"
*             MOVE     "END"      TO   END-FLG
*             GO                  TO   DEXXXXL2-READ-EXIT
*    END-IF.
 READ040.
*取込日付チェック
*    IF       DEX-F02  NOT =  PARA-TRDATE
*    OR       DEX-F03  NOT =  PARA-TRTIME
*             MOVE     "END"      TO   END-FLG
*             GO                  TO   DEXXXXL2-READ-EXIT
*    END-IF.
*
 DEXXXXL2-READ-EXIT.
     EXIT.
******************************************************************
*                  伝票番号取得
******************************************************************
 DPNO-SET-SEC                 SECTION.
     MOVE     "DPNO-SET-SEC"       TO   S-NAME.
*
     INITIALIZE                        LINK-AREA-SUB1.
*
 DPNO-010.
     MOVE      DEX-F204       TO        LI-TRCD.
*↓TEST
*    DISPLAY   "URI-F01="  URI-F01   UPON CONS.
*    DISPLAY   "LI-TRCD="  LI-TRCD   UPON CONS.
*↑TEST
     CALL     "SKYSBCK1"     USING     LINK-AREA-SUB1.
 DPNO-020.
*↓TEST
*    DISPLAY   "LO-ERR ="  LO-ERR    UPON CONS.
*    DISPLAY   "LO-DENO="  LO-DENNO  UPON CONS.
*    DISPLAY   "LO-NEXT="  LO-NEXT   UPON CONS.
*↑TEST
     IF       LO-ERR  =  0
              MOVE      LO-NEXT     TO        OUT-DENNO
              MOVE      SPACE       TO        TOK-INV-FLG
     ELSE
              DISPLAY   NC"伝票_採番エラー"  UPON CONS
              DISPLAY   LI-TRCD               UPON CONS
              MOVE      "INV"       TO        TOK-INV-FLG
              MOVE      4001        TO        PROGRAM-STATUS
*             ACCEPT    IN-DATA     FROM      CONS
              STOP  RUN
     END-IF.
*
 DPNO-SET-EXIT.
     EXIT.
****************************************************************
*                商品変換テーブル読込み                        *
****************************************************************
 HSHOTBL-READ-SEC          SECTION.
*
     MOVE      "HSHOTBL-READ-SEC" TO    S-NAME.
*
     MOVE      DEX-F204    TO     TBL-F01.
     MOVE      DEX-F216    TO     TBL-F02.
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
     MOVE      SPACE       TO     HMEIMS-INV-FLG.
*
     MOVE      TBL-F031    TO     MEI-F011.
     MOVE      TBL-F032    TO     MEI-F012.
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
*                取引先マスタ　　　　　                        *
****************************************************************
 HTOKMS-READ-SEC           SECTION.
*
     MOVE      "HTOKMS-READ-SEC"  TO    S-NAME.
*
     MOVE      DEX-F204    TO     TOK-F01.
     READ      HTOKMS
               INVALID
               MOVE      "INV"    TO    HTOKMS-INV-FLG
               NOT  INVALID
               MOVE      SPACE    TO    HTOKMS-INV-FLG
     END-READ.
*
 HTOKMS-READ-EXIT.
     EXIT.
****************************************************************
*                店舗マスタ読込　　　　                        *
****************************************************************
 HTENMS-READ-SEC           SECTION.
*
     MOVE      "HTENMS-READ-SEC"  TO    S-NAME.
*
     MOVE      DEX-F204    TO     TEN-F52.
     MOVE      DEX-F208    TO     TEN-F011.
     READ      HTENMS
               INVALID
               MOVE      "INV"    TO    HTENMS-INV-FLG
               NOT  INVALID
               MOVE      SPACE    TO    HTENMS-INV-FLG
     END-READ.
*
 HTENMS-READ-EXIT.
     EXIT.
****************************************************************
*                ナフコ商品マスタ読込　                        *
****************************************************************
*NFSHOMS-READ-SEC          SECTION.
*
*    MOVE      "NFSHOMS-READ-SEC" TO    S-NAME.
*
*    MOVE      DEX-F11C    TO     NFM-F01.
*    READ      NFSHOMS
*              INVALID
*              MOVE      "INV"    TO    NFSHOMS-INV-FLG
*              NOT  INVALID
*              MOVE      SPACE    TO    NFSHOMS-INV-FLG
*    END-READ.
*
*NFSHOMS-INV-EXIT.
*    EXIT.
*#2020/12/18 NAV ST
****************************************************************
*                ＳＵＢ商品変換ＴＢＬ読込                      *
****************************************************************
 SUBTBLF-READ-SEC          SECTION.
*
     MOVE      "HSHOTBL-READ-SEC" TO    S-NAME.
*
     MOVE      DEX-F204    TO     SUB-F01.
     MOVE      DEX-F216    TO     SUB-F02.
     READ      SUBTBLF
               INVALID
               MOVE      "INV"    TO    SUBTBLF-INV-FLG
               NOT  INVALID
               MOVE      SPACE    TO    SUBTBLF-INV-FLG
     END-READ.
*
 SUBTBLF-READ-EXIT.
     EXIT.
*#2020/12/18 NAV ED
****************************************************************
*     Ｄ３６５伝票番号採番　　　　　　　                       *
****************************************************************
 D365-DEN-SEC              SECTION.
*
     MOVE      "NFSHOMS-READ-SEC" TO    S-NAME.
*
     INITIALIZE                   WK-D365-DEN-PARA.
*****伝票区分でＤ３６５伝票番号の先頭１桁を変更
     IF   DEX-F207  =  "40"
          MOVE "T"         TO     WK-D365-PARA-IN1
     ELSE
          IF   DEX-F207  =  "41"
               MOVE "H"    TO     WK-D365-PARA-IN1
          ELSE
               MOVE "T"    TO     WK-D365-PARA-IN1
          END-IF
     END-IF.
     MOVE      DEX-F204    TO     WK-D365-PARA-IN2.
     CALL      "SKYD3DEN"  USING  WK-D365-PARA-IN1
                                  WK-D365-PARA-IN2
                                  WK-D365-PARA-OUT1
                                  WK-D365-PARA-OUT2.
     IF   WK-D365-PARA-OUT2  =  "1"
          DISPLAY "## " NC"異常" "D365NO" NC"採番" " ERR]] ##"
          MOVE   4000      TO      PROGRAM-STATUS
          STOP  RUN
     END-IF.
*
 D365-DEN-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*#2017/03/14 NAV ST 伝票番号採番時、伝票番号をカウントアップ
     IF   DPNO-SET-FLG  =  "SET"
          MOVE      DPNO-TOKCD   TO   DEX-F204
          PERFORM   DPNO-SET-SEC
     END-IF.
*#2017/03/14 NAV ED
*件数印字
*伝票EXCEL取込ファイル読込
     DISPLAY "DEXXXXL2   READ CNT = " DEXXXXL2-READ-CNT UPON CONS.
*取込エラーカウント
     DISPLAY "ERROR           CNT = " ERR-CNT           UPON CONS.
*手書伝票基本データ
     DISPLAY "URIXXXF    WRT  CNT = " DEN-ADD-CNT       UPON CONS.
*
     CLOSE    DEXXXXL2  HSHOTBL  HMEIMS  HTOKMS
              HTENMS    URIXXXF.
*#2020/12/18 NAV ST
     CLOSE    SUBTBLF.
*#2020/12/18 NAV ED
*
     STOP     RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
