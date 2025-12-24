****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ＨＧ基幹　                        *
*    業務名　　　　　　　：　社内振替　　　　　　　　　        *
*    モジュール名　　　　：　社内振替明細入力　                *
*    流用元　　　　　　　：　SFU3040I+SNJ0910I                 *
*    作成日／作成者　　　：　2017/02/07 INOUE                  *
*    更新日／更新者　　　：　2017/04/14 TAKAHASHI              *
*                            在庫更新機能を追加                *
*    更新日／更新者　　　：　2017/04/18 INOUE                  *
*                            完納区分＝１の場合は入力不可とする*
*                            発注・入荷（登録・修正・削除）全て*
*    更新日／更新者　　　：　2017/05/01 TAKAHASHI              *
*                            発注時、発注残数（未入庫数）を更新*
*                            する。　　　　　　　　　　　　　　*
*    更新日／更新者　　　：　2018/05/01 T.TAKAHASHI            *
*    更新内容　　　　　　：　伝票区分＝７０固定                *
*    更新日／更新者　　　：　2018/06/15 T.TAKAHASHI            *
*    更新内容　　　　　　：　当月／次月在庫更新障害修正        *
****************************************************************
 IDENTIFICATION            DIVISION.
 PROGRAM-ID.               SFU3050I.
 AUTHOR.                   NAV.
 DATE-WRITTEN.             2017/02/07.
 ENVIRONMENT               DIVISION.
 CONFIGURATION             SECTION.
 SOURCE-COMPUTER.          PG6000.
 OBJECT-COMPUTER.          PG6000.
 SPECIAL-NAMES.
     CONSOLE     IS        CONS.
***************************************************************
 INPUT-OUTPUT              SECTION.
***************************************************************
 FILE-CONTROL.
*振替明細ファイル
     SELECT      SFRMEIL1  ASSIGN    TO        DA-01-VI-SFRMEIL1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      DYNAMIC
                           RECORD    KEY       MEI-F01
                                               MEI-F02
                                               MEI-F03
                                               MEI-F04
                                               MEI-F05
                                               MEI-F06
                                               MEI-F07
                                               MEI-F08
                                               MEI-F11
                                               MEI-F12
                                               WITH DUPLICATES
                           FILE      STATUS    MEI-ST.
*振替情報ファイル
     SELECT      SFRHEDL1  ASSIGN    TO        DA-01-VI-SFRHEDL1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      DYNAMIC
                           RECORD    KEY       HED-F01
                                               HED-F02
                                               HED-F03
                                               HED-F04
                                               HED-F05
                                               HED-F06
                                               HED-F07
                                               HED-F08
                           FILE      STATUS    HED-ST.
*倉庫マスタ
     SELECT      ZSOKMS1   ASSIGN    TO        DA-01-VI-ZSOKMS1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      DYNAMIC
                           RECORD    KEY       SOK-F01
                           FILE      STATUS    SOK-ST.
*担当者マスタ
     SELECT      TANMS1    ASSIGN    TO        DA-01-VI-TANMS1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      DYNAMIC
                           RECORD    KEY       TAN-F01
                                               TAN-F02
                           FILE      STATUS    TAN-ST.
*条件ファイル
     SELECT      JYOKEN1   ASSIGN    TO        DA-01-VI-JYOKEN1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       JYO-F01
                                               JYO-F02
                           FILE      STATUS    JYO-ST.
*シーズンマスタ
     SELECT      SEASONL1  ASSIGN    TO        DA-01-VI-SEASONL1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       SEA-F01
                           FILE      STATUS    SEA-ST.
*仕入先マスタ
     SELECT      ZSHIMS1   ASSIGN    TO        DA-01-VI-ZSHIMS1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       SHI-F01
                           FILE      STATUS    SHI-ST.
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
*画面ファイル*
     SELECT      DSPFILE   ASSIGN    TO        GS-DSPF
                           SYMBOLIC  DESTINATION        "DSP"
                           DESTINATION-1       DSP-WS
                           FORMAT              DSP-FMT
                           GROUP               DSP-GRP
                           PROCESSING  MODE    DSP-PRO
                           UNIT      CONTROL   DSP-UNIT
                           SELECTED  FUNCTION  DSP-FNC
                           FILE      STATUS    DSP-ST.
******************************************************************
 DATA                      DIVISION.
******************************************************************
 FILE                      SECTION.
*振替明細ファイル
 FD  SFRMEIL1
     LABEL       RECORD    IS        STANDARD.
     COPY        SFRMEIL1  OF        XFDLIB
     JOINING     MEI       AS        PREFIX.
*振替情報ファイル
 FD  SFRHEDL1
     LABEL       RECORD    IS        STANDARD.
     COPY        SFRHEDL1  OF        XFDLIB
     JOINING     HED       AS        PREFIX.
*倉庫マスタ
 FD  ZSOKMS1
     LABEL       RECORD    IS        STANDARD.
     COPY        ZSOKMS1   OF        XFDLIB
     JOINING     SOK       AS        PREFIX.
*担当者マスタ
 FD  TANMS1
     LABEL       RECORD    IS        STANDARD.
     COPY        TANMS1    OF        XFDLIB
     JOINING     TAN       AS        PREFIX.
*条件ファイル
 FD  JYOKEN1
     LABEL       RECORD    IS        STANDARD.
     COPY        JYOKEN1   OF        XFDLIB
     JOINING     JYO       AS        PREFIX.
*シーズンマスタ
 FD  SEASONL1
     LABEL       RECORD    IS        STANDARD.
     COPY        SEASONL1  OF        XFDLIB
     JOINING     SEA       AS        PREFIX.
*仕入先マスタ
 FD  ZSHIMS1
     LABEL       RECORD    IS        STANDARD.
     COPY        ZSHIMS1   OF        XFDLIB
     JOINING     SHI       AS        PREFIX.
*商品在庫マスタ
 FD  ZAMZAIL1.
     COPY        ZAMZAIF   OF        XFDLIB
                 JOINING   ZAI       PREFIX.
*商品名称マスタ
 FD  MEIMS1.
     COPY        HMEIMS    OF        XFDLIB
                 JOINING   MES       PREFIX.
*画面ファイル
 FD  DSPFILE.
     COPY        FFU30501  OF        XMDLIB.
******************************************************************
 WORKING-STORAGE        SECTION.
******************************************************************
 01  DSP-AREA.
     03  DSP-FMT           PIC X(08) VALUE     SPACE.
     03  DSP-GRP           PIC X(08) VALUE     SPACE.
     03  DSP-WS            PIC X(08) VALUE     SPACE.
     03  DSP-WSR           REDEFINES DSP-WS.
         05  DSP-WS1       PIC X(02).
         05  DSP-WS2       PIC 9(03).
         05  DSP-WS3       PIC X(01).
     03  DSP-PRO           PIC X(02) VALUE     SPACE.
     03  DSP-UNIT          PIC X(06) VALUE     SPACE.
     03  DSP-FNC           PIC X(04) VALUE     SPACE.
     03  DSP-ST            PIC X(02) VALUE     SPACE.
     03  DSP-ST1           PIC X(04) VALUE     SPACE.
*--------------------------------------------------------*
 01  MSG-AREA.
     03  MSG01           PIC N(20) VALUE
                         NC"登録されていません".
     03  MSG02           PIC N(20) VALUE
                         NC"すでに登録されています".
     03  MSG03           PIC N(20) VALUE
                         NC"シーズンを入力してください".
     03  MSG04           PIC N(20) VALUE
                         NC"伝票区分を入力してください".
     03  MSG05           PIC N(20) VALUE
                         NC"処理区分が違います".
     03  MSG06           PIC N(20) VALUE
                         NC"Ｙで入力してください".
     03  MSG07           PIC N(20) VALUE
                         NC"倉庫ＣＤを入力してください".
     03  MSG08           PIC N(20) VALUE
                         NC"年度（前後２年）を入力してください".
     03  MSG09           PIC N(20) VALUE
                         NC"倉庫Ｍが登録されていません".
     03  MSG10           PIC N(20) VALUE
                         NC"条件ファイル未登録の部門です".
     03  MSG11           PIC N(20) VALUE
                         NC"シーズンマスタ未登録です".
     03  MSG12           PIC N(20) VALUE
                         NC"商品コードを入力してください".
*    03  MSG13           PIC N(20) VALUE
*                        NC"商品名称マスタ未登録です".
     03  MSG14           PIC N(20) VALUE
                         NC"入荷予定日を入力してください".
     03  MSG15           PIC N(20) VALUE
                         NC"仕入先マスタ未登録です".
     03  MSG16           PIC N(20) VALUE
                         NC"完納区分は、空白または１です".
     03  MSG17           PIC N(20) VALUE
                         NC"日付が不正です".
     03  MSG18           PIC N(20) VALUE
                         NC"ＰＦキーが違います".
     03  MSG19           PIC N(20) VALUE
                         NC"発注日を入力してください".
     03  MSG20           PIC N(20) VALUE
                         NC"入荷日を入力してください".
     03  MSG21           PIC N(20) VALUE
                         NC"発注数を入力してください".
     03  MSG22           PIC N(20) VALUE
                         NC"入荷数を入力してください".
*↓2017/04/18
     03  MSG23           PIC N(20) VALUE
                         NC"完納済のため処理できません".
*↑2017/04/18
*--------------------------------------------------------*
     03  PMSG01            PIC N(20)
            VALUE  NC"_終了".
     03  PMSG02            PIC N(20)
            VALUE  NC"_取消".
     03  PMSG03            PIC N(20)
            VALUE  NC"_取消　_再入力".
     03  PMSG06            PIC N(20)
            VALUE  NC"_明細照会　_取消　_再入力".
*--------------------------------------------------------*
*日付／時刻
 01  WSYS-DATE.
     03  WSYS-Y1           PIC 9(02).
     03  WSYS-YMD.
         05  WSYS-YY       PIC 9(02).
         05  WSYS-MM       PIC 9(02).
         05  WSYS-DD       PIC 9(02).
 01  TIME-AREA.
     03  WK-TIME                  PIC  9(08)  VALUE  ZERO.
 01  DATE-AREA.
     03  WK-YS                    PIC  9(02)  VALUE  ZERO.
     03  WK-DATE.
         05  WK-Y                 PIC  9(02)  VALUE  ZERO.
         05  WK-M                 PIC  9(02)  VALUE  ZERO.
         05  WK-D                 PIC  9(02)  VALUE  ZERO.
 01  DATE-AREAR2       REDEFINES      DATE-AREA.
     03  SYS-DATE                 PIC  9(08).
*画面表示日付編集
 01  HEN-DATE.
     03  HEN-DATE-YYYY            PIC  9(04)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-DD              PIC  9(02)  VALUE  ZERO.
*画面表示時刻編集
 01  HEN-TIME.
     03  HEN-TIME-HH              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-SS              PIC  9(02)  VALUE  ZERO.
*--------------------------------------------------------*
 01  WK-AREA.
     03  END-FLG           PIC 9(01).
     03  INV-SW            PIC 9(01) VALUE     ZERO.
     03  ZI-FLG            PIC 9(01) VALUE     ZERO.
     03  WK-SYUKKA         PIC X(08) VALUE     SPACE.
     03  ERR-FLG           PIC 9(01) VALUE     ZERO.
     03  WK-SYORI          PIC 9(01) VALUE     ZERO.
     03  ZSOKMS1-INV-FLG   PIC X(03) VALUE     SPACE.
     03  TANMS1-INV-FLG    PIC X(03) VALUE     SPACE.
     03  JYOKEN1-INV-FLG   PIC X(03) VALUE     SPACE.
     03  SEASON1-INV-FLG   PIC X(03) VALUE     SPACE.
     03  HMEIMS-INV-FLG    PIC X(03) VALUE     SPACE.
     03  ZSHIMS1-INV-FLG   PIC X(03) VALUE     SPACE.
*--------------------------------------------------------*
 01  ST-AREA.
     03  IN-DATA           PIC X(01).
     03  HED-ST            PIC X(02).
     03  MEI-ST            PIC X(02).
     03  SOK-ST            PIC X(02).
     03  TAN-ST            PIC X(02).
     03  JYO-ST            PIC X(02).
     03  SEA-ST            PIC X(02).
*    03  SHO-ST            PIC X(02).
     03  SHI-ST            PIC X(02).
     03  ZAI-ST            PIC X(02).
     03  MES-ST            PIC X(02).
*--------------------------------------------------------*
     03  FILE-ERR1         PIC N(10) VALUE
                                  NC"画面ファイル異常！".
     03  FILE-ERR2         PIC N(11) VALUE
                                  NC"振替情報ファイル異常！".
     03  FILE-ERR3         PIC N(11) VALUE
                                  NC"振替明細ファイル異常！".
     03  FILE-ERR4         PIC N(10) VALUE
                                  NC"倉庫マスタ異常！".
     03  FILE-ERR5         PIC N(10) VALUE
                                  NC"担当者マスタ異常！".
     03  FILE-ERR6         PIC N(10) VALUE
                                  NC"条件ファイル異常！".
     03  FILE-ERR7         PIC N(10) VALUE
                                  NC"シーズンマスタ異常！".
*    03  FILE-ERR8         PIC N(10) VALUE
*                                 NC"商品名称マスタ異常！".
     03  FILE-ERR9         PIC N(10) VALUE
                                  NC"仕入先マスタ異常！".
     03  FILE-ERR10        PIC N(10) VALUE
                                  NC"在庫マスタ異常！".
     03  FILE-ERR11        PIC N(10) VALUE
                                  NC"商品名称マスタ異常！".
*--------------------------------------------------------*
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*--------------------------------------------------------*
*レコード退避エリア
 01  DATA-TAIHI              PIC  X(300) VALUE SPACE.
*--------------------------------------------------------*
*振替情報ファイル更新用ワーク
 01  SUM-HACDAT              PIC  9(08) VALUE ZERO.
 01  SUM-HACGK               PIC S9(09) VALUE ZERO.
 01  SUM-HACBUMON            PIC  X(04) VALUE SPACE.
 01  SUM-HACTANTO            PIC  X(02) VALUE SPACE.
 01  SUM-NYKDAT              PIC  9(08) VALUE ZERO.
 01  SUM-NYKGK               PIC S9(09) VALUE ZERO.
 01  SUM-NYKBUMON            PIC  X(04) VALUE SPACE.
 01  SUM-NYKTANTO            PIC  X(02) VALUE SPACE.
*--------------------------------------------------------*
*画面退避エリア（更新後非消去項目）
 01  TAIHI-PGID              PIC  X(08) VALUE SPACE.
 01  TAIHI-FORMID            PIC  X(08) VALUE SPACE.
 01  TAIHI-HACNYK            PIC  N(04) VALUE SPACE.
 01  TAIHI-SDATE             PIC  X(10) VALUE SPACE.
 01  TAIHI-STIME             PIC  X(08) VALUE SPACE.
 01  TAIHI-SYORI             PIC  X(01) VALUE SPACE.
 01  TAIHI-SYKNM             PIC  N(02) VALUE SPACE.
 01  TAIHI-MTANTO            PIC  X(07) VALUE SPACE.
 01  TAIHI-TANNM             PIC  N(05) VALUE SPACE.
 01  TAIHI-DENKCD            PIC  9(02) VALUE ZERO.
 01  TAIHI-DENKNM            PIC  N(04) VALUE SPACE.
 01  TAIHI-NENDO             PIC  9(04) VALUE ZERO.
 01  TAIHI-SEASON            PIC  X(02) VALUE SPACE.
 01  TAIHI-SEASNM            PIC  N(10) VALUE SPACE.
 01  TAIHI-SOKCD             PIC  X(02) VALUE SPACE.
 01  TAIHI-SOKNM             PIC  N(10) VALUE SPACE.
 01  TAIHI-SKTCD             PIC  X(08) VALUE SPACE.
 01  TAIHI-HINT1             PIC  X(05) VALUE SPACE.
 01  TAIHI-HINT2             PIC  X(02) VALUE SPACE.
 01  TAIHI-HINT3             PIC  X(01) VALUE SPACE.
 01  TAIHI-MSG1              PIC  N(04) VALUE SPACE.
 01  TAIHI-MSG2              PIC  N(04) VALUE SPACE.
 01  TAIHI-MSG3              PIC  N(04) VALUE SPACE.
*--------------------------------------------------------*
 01  SEQ                   PIC  9(02).
*--------------------------------------------------------*
*下位ＰＧ呼出パラメタ
*
*【社内振替情報照会（明細）】
 01  SFU3070I-PARA.
* 伝票区分
   03  SFU3070I-DENKU        PIC  X(02) VALUE SPACE.
* 年度
   03  SFU3070I-NENDO        PIC  9(04) VALUE ZERO.
* シーズン
   03  SFU3070I-SEASON       PIC  X(02) VALUE SPACE.
* 倉庫ＣＤ
   03  SFU3070I-SOUKO        PIC  X(02) VALUE SPACE.
* 商品ＣＤ
   03  SFU3070I-SHOHIN       PIC  X(08) VALUE SPACE.
* 品単１
   03  SFU3070I-HINTAN1      PIC  X(05) VALUE SPACE.
* 品単２
   03  SFU3070I-HINTAN2      PIC  X(02) VALUE SPACE.
* 品単３
   03  SFU3070I-HINTAN3      PIC  X(01) VALUE SPACE.
* 仕入先ＣＤ
   03  SFU3070I-SHIIRE       PIC  9(08) VALUE ZERO.
* 発注数合計
   03  SFU3070I-HACKEI       PIC S9(09) VALUE ZERO.
* 入荷数合計
   03  SFU3070I-NYUKEI       PIC S9(09) VALUE ZERO.
*--------------------------------------------------------*
*呼出前モード退避
 01  BK-SYORI              PIC X(01) VALUE SPACE.
*初期完納区分退避
 01  BK-KANKBN             PIC X(01) VALUE SPACE.
 01  BK-KANBMN             PIC X(04) VALUE SPACE.
 01  BK-KANTAN             PIC X(02) VALUE SPACE.
 01  BK-KANDAT             PIC 9(08) VALUE ZERO.
*--------------------------------------------------------*
*前後２年度
 01  NENDO-BEFORE          PIC   9(04)  VALUE  ZERO.
 01  NENDO-AFTER           PIC   9(04)  VALUE  ZERO.
*--------------------------------------------------------*
*復帰制御用ＦＬＧ
*01  SFU3050I-SEL          PIC X(01) VALUE SPACE.
*--------------------------------------------------------*
*ＰＦ４押下判別
 01  PF4-SEL               PIC X(01) VALUE SPACE.
*--------------------------------------------------------*
*プログラムスイッチ
 01  PSW                   PIC X(01) VALUE SPACE.
*--------------------------------------------------------*
*#2017/04/14 NAV ST -------------------------------------*
 01  WK-BAK-TANBAN         PIC X(06) VALUE  SPACE.
 01  WK-BAK-HNSU           PIC S9(07) VALUE  ZERO.
 01  WK-BAK-HNDATE         PIC 9(06) VALUE  ZERO.
 01  WK-KEY-SOKCD          PIC X(02) VALUE  SPACE.
 01  WK-KEY-SYOCD          PIC X(08) VALUE  SPACE.
 01  WK-KEY-HINCD          PIC X(08) VALUE  SPACE.
 01  WK-KEY-TANBAN         PIC X(06) VALUE  SPACE.
 01  WK-SURYO              PIC S9(07) VALUE  ZERO.
 01  WK-SURYOZAN           PIC S9(07) VALUE  ZERO.
****  ＷＲＫ領域  ***
 01  WRK-AREA.
     02  WRK-DATE1           PIC  9(08).
     02  WRK-DATE1R          REDEFINES   WRK-DATE1.
         04  WRK-DATE1R1     PIC  9(06).
         04  WRK-DATE1R2     PIC  9(02).
     02  WRK-DATE2           PIC  9(06).
*#2017/04/14 NAV ED -------------------------------------*
*
 LINKAGE                   SECTION.
*   呼出区分
 01  PARA-CALL             PIC X(01).
*   実行種別
 01  PARA-SYUBETU          PIC X(01).
*   処理モード
 01  PARA-SYORI            PIC X(01).
*   部門
 01  PARA-BUMONCD          PIC X(04).
*   担当者
 01  PARA-TANCD            PIC X(02).
*   伝票区分
 01  PARA-DENKU            PIC X(02).
*   年度
 01  PARA-NENDO            PIC 9(04).
*   シーズン
 01  PARA-SEASON           PIC X(02).
*   倉庫
 01  PARA-SOUKO            PIC X(02).
*   商品ＣＤ
 01  PARA-SHOHIN           PIC X(08).
*   品単１
 01  PARA-HINTAN1          PIC X(05).
*   品単２
 01  PARA-HINTAN2          PIC X(02).
*   品単３
 01  PARA-HINTAN3          PIC X(01).
*--------------------------------------------------------*
******************************************************************
 PROCEDURE                 DIVISION  USING PARA-CALL
                                           PARA-SYUBETU
                                           PARA-SYORI
                                           PARA-BUMONCD
                                           PARA-TANCD
                                           PARA-DENKU
                                           PARA-NENDO
                                           PARA-SEASON
                                           PARA-SOUKO
                                           PARA-SHOHIN
                                           PARA-HINTAN1
                                           PARA-HINTAN2
                                           PARA-HINTAN3.
******************************************************************
*                     ファイルエラー処理
******************************************************************
 DECLARATIVES.
 DSP-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE           DSPFILE.
     DISPLAY     DSP-ST    UPON      CONS.
     DISPLAY     FILE-ERR1 UPON      CONS.
     ACCEPT      IN-DATA   FROM      CONS.
     MOVE        4000      TO        PROGRAM-STATUS.
     EXIT PROGRAM.
 MEI-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE           SFRMEIL1.
     DISPLAY     MEI-ST    UPON      CONS.
     DISPLAY     FILE-ERR3 UPON      CONS.
     ACCEPT      IN-DATA   FROM      CONS.
     MOVE        4000      TO        PROGRAM-STATUS.
     EXIT PROGRAM.
 HED-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE           SFRHEDL1.
     DISPLAY     HED-ST    UPON      CONS.
     DISPLAY     FILE-ERR2 UPON      CONS.
     ACCEPT      IN-DATA   FROM      CONS.
     MOVE        4000      TO        PROGRAM-STATUS.
     EXIT PROGRAM.
 SOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE           ZSOKMS1.
     DISPLAY     SOK-ST    UPON      CONS.
     DISPLAY     FILE-ERR4 UPON      CONS.
     ACCEPT      IN-DATA   FROM      CONS.
     MOVE        4000      TO        PROGRAM-STATUS.
     EXIT PROGRAM.
 TAN-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE           TANMS1.
     DISPLAY     TAN-ST    UPON      CONS.
     DISPLAY     FILE-ERR5 UPON      CONS.
     ACCEPT      IN-DATA   FROM      CONS.
     MOVE        4000      TO        PROGRAM-STATUS.
     EXIT PROGRAM.
 JYO-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE           JYOKEN1.
     DISPLAY     JYO-ST    UPON      CONS.
     DISPLAY     FILE-ERR6 UPON      CONS.
     ACCEPT      IN-DATA   FROM      CONS.
     MOVE        4000      TO        PROGRAM-STATUS.
     EXIT PROGRAM.
 SEA-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE SEASONL1.
     DISPLAY     SEA-ST    UPON      CONS.
     DISPLAY     FILE-ERR7 UPON      CONS.
     ACCEPT      IN-DATA   FROM      CONS.
     MOVE        4000      TO        PROGRAM-STATUS.
     EXIT PROGRAM.
 SHI-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE ZSHIMS1.
     DISPLAY     SHI-ST    UPON      CONS.
     DISPLAY     FILE-ERR9 UPON      CONS.
     ACCEPT      IN-DATA   FROM      CONS.
     MOVE        4000      TO        PROGRAM-STATUS.
     EXIT PROGRAM.
 ZAI-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE ZAMZAIL1.
     DISPLAY     ZAI-ST    UPON      CONS.
     DISPLAY    FILE-ERR10 UPON      CONS.
     ACCEPT      IN-DATA   FROM      CONS.
     MOVE        4000      TO        PROGRAM-STATUS.
     EXIT PROGRAM.
 MEI-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE MEIMS1.
     DISPLAY     SHI-ST    UPON      CONS.
     DISPLAY    FILE-ERR11 UPON      CONS.
     ACCEPT      IN-DATA   FROM      CONS.
     MOVE        4000      TO        PROGRAM-STATUS.
     EXIT PROGRAM.
 END DECLARATIVES.
******************************************************************
*                                                                *
******************************************************************
 SHORI-SEC                 SECTION.
     PERFORM     INIT-SEC.
     PERFORM     MAIN-SEC  UNTIL     END-FLG   =   9.
     PERFORM     END-SEC.
     EXIT PROGRAM.
 SHORI-EXIT.
     EXIT.
******************************************************************
*                      初期処理
******************************************************************
 INIT-SEC                  SECTION.
*
     OPEN        INPUT     ZSOKMS1   JYOKEN1  TANMS1
                           SEASONL1  MEIMS1
                           ZSHIMS1.
     OPEN        I-O       SFRHEDL1  SFRMEIL1
                           DSPFILE.
*#2017/04/14 NAV ST
     OPEN        I-O       ZAMZAIL1.
*#2017/04/14 NAV ED
     MOVE        ZERO      TO        END-FLG.
     MOVE        SPACE     TO        PF4-SEL.
     MOVE        SPACE     TO        MEI-REC.
     INITIALIZE  MEI-REC.
*----------------- 99/09/28追加 ---------------------*
*システム日付・時刻の取得
     ACCEPT   WK-DATE           FROM   DATE.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     WK-DATE             TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD       TO   DATE-AREA.
*画面表示日付編集
     MOVE      SYS-DATE(1:4)      TO   HEN-DATE-YYYY.
     MOVE      SYS-DATE(5:2)      TO   HEN-DATE-MM.
     MOVE      SYS-DATE(7:2)      TO   HEN-DATE-DD.
*システム日付取得
     ACCEPT    WK-TIME          FROM   TIME.
*画面表示時刻編集
     MOVE      WK-TIME(1:2)       TO   HEN-TIME-HH.
     MOVE      WK-TIME(3:2)       TO   HEN-TIME-MM.
     MOVE      WK-TIME(5:2)       TO   HEN-TIME-SS.
*#2017/04/14 NAV ST
*---<  日付の取得  >---*
     MOVE    "99"            TO   JYO-F01.
     MOVE    "ZAI"           TO   JYO-F02.
     READ    JYOKEN1
             INVALID     MOVE "INV" TO JYOKEN1-INV-FLG
             NOT INVALID MOVE SPACE TO JYOKEN1-INV-FLG
     END-READ.
     IF  JYOKEN1-INV-FLG  =  SPACE
*********MOVE    JYO-F05     TO   WRK-DATE1
         MOVE    JYO-F04     TO   WRK-DATE1
     ELSE
         DISPLAY NC"＃＃条件Ｆ（月次日付）エラー＃＃" UPON CONS
         STOP  RUN
     END-IF.
*#2017/04/14 NAV ED
*
     MOVE     "1"                 TO   PSW.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*                      メイン処理                              *
****************************************************************
 MAIN-SEC                  SECTION.
*
     IF      PSW     =   "1"
             CONTINUE
     ELSE
             GO                TO        MAIN-020
     END-IF.
*
*初期画面表示
 MAIN-000.
     PERFORM     DSP-INIT-SEC.
*
*ＰＧからＣＡＬＬの場合はスキップ
     IF       (  PARA-CALL  =        "P"  )  AND
              (  PF4-SEL    =        " "  )
                 MOVE       PMSG02       TO        PFMSG
                 MOVE       "PFMSG"      TO        DSP-GRP
                 PERFORM    DSP-WR-SEC
                 GO         TO       MAIN-021
     END-IF.
*
*処理区分入力
 MAIN-010.
     MOVE        PMSG01     TO       PFMSG.
     MOVE       "PFMSG"     TO       DSP-GRP.
     PERFORM     DSP-WR-SEC.
     MOVE       "SYORI"     TO       DSP-GRP.
     PERFORM     DSP-RD-SEC.
     EVALUATE    DSP-FNC
       WHEN
        "F005"
           MOVE     9      TO        END-FLG
           GO              TO        MAIN-EXIT
     END-EVALUATE.
     PERFORM     DSP-WR-SEC.
*処理区分 CHK
     IF  ( SYORI  =  1 OR 2 OR 3 )
          MOVE    SPACE     TO        ERRMSG
          MOVE   "ERRMSG"   TO        DSP-GRP
          PERFORM DSP-WR-SEC
          MOVE   "M"        TO        EDIT-OPTION  OF  SYORI
          MOVE    SPACE     TO        EDIT-CURSOR  OF  SYORI
          IF      SYORI  =  1
                  MOVE   NC"登録"     TO            SYKNM
          END-IF
          IF      SYORI  =  2
                  MOVE   NC"修正"     TO            SYKNM
          END-IF
          IF      SYORI  =  3
                  MOVE   NC"削除"     TO            SYKNM
          END-IF
*
          MOVE   "MODE"     TO        DSP-GRP
          PERFORM DSP-WR-SEC
*#2018/05/01 NAV ST 伝票区分=70固定
          MOVE   70         TO        DENKCD
          MOVE   "DENKCD"   TO        DSP-GRP
          PERFORM DSP-WR-SEC
*#2018/05/01 NAV ED 伝票区分=70固定
       ELSE
         MOVE    MSG05     TO        ERRMSG
         MOVE   "ERRMSG"   TO        DSP-GRP
         PERFORM DSP-WR-SEC
         MOVE   "R"        TO        EDIT-OPTION  OF  SYORI
         MOVE   "C"        TO        EDIT-CURSOR  OF  SYORI
         MOVE   "SYORI"    TO        DSP-GRP
         PERFORM DSP-WR-SEC
         GO                  TO        MAIN-010
     END-IF.
 MAIN-020.
*  ＰＦキーガイダンス
     IF  (SYORI  =  2)
          MOVE   PMSG02      TO        PFMSG
     ELSE
          MOVE   PMSG02      TO        PFMSG
     END-IF.
     MOVE       "PFMSG"      TO        DSP-GRP.
     PERFORM     DSP-WR-SEC.
*ＰＧからＣＡＬＬの場合はスキップ
     IF       (  PARA-CALL  =        "P"  )
                 GO         TO       MAIN-021
     END-IF.
*
*  キー情報入力
     MOVE       "HEAD01"     TO        DSP-GRP.
     PERFORM     DSP-RD-SEC.
     EVALUATE    DSP-FNC
       WHEN
        "F004"
           MOVE  "1"         TO        PSW
           MOVE  "Y"         TO        PF4-SEL
           GO                TO        MAIN-SEC
       WHEN
        "E000"
           CONTINUE
       WHEN
         OTHER
           MOVE    MSG18     TO        ERRMSG
           MOVE   "ERRMSG"   TO        DSP-GRP
           PERFORM DSP-WR-SEC
           GO                TO        MAIN-020
     END-EVALUATE.
     PERFORM     DSP-WR-SEC.
 MAIN-021.
*入力値チェック
*  伝票区分
     IF ( DENKCD  =  ZERO )  OR  ( DENKCD  NOT NUMERIC )
         MOVE    MSG04       TO        ERRMSG
         MOVE   "ERRMSG"     TO        DSP-GRP
         PERFORM DSP-WR-SEC
         MOVE   "R"          TO        EDIT-OPTION  OF  DENKCD
         MOVE   "C"          TO        EDIT-CURSOR  OF  DENKCD
         MOVE   "HEAD01"     TO        DSP-GRP
         PERFORM DSP-WR-SEC
         GO                  TO        MAIN-020
     ELSE
         MOVE   "M"          TO        EDIT-OPTION  OF  DENKCD
         MOVE    SPACE       TO        EDIT-CURSOR  OF  DENKCD
         MOVE   "HEAD01"     TO        DSP-GRP
         PERFORM DSP-WR-SEC
     END-IF.
*
     MOVE        1           TO        JYO-F01.
     MOVE        DENKCD      TO        JYO-F02.
     READ        JYOKEN1
       INVALID
         MOVE    SPACE       TO        DENKNM
         MOVE    MSG10       TO        ERRMSG
         MOVE   "ERRMSG"     TO        DSP-GRP
         PERFORM DSP-WR-SEC
         MOVE   "R"          TO        EDIT-OPTION  OF  DENKCD
         MOVE   "C"          TO        EDIT-CURSOR  OF  DENKCD
         MOVE   "HEAD01"     TO        DSP-GRP
         PERFORM DSP-WR-SEC
         GO                  TO        MAIN-020
       NOT INVALID
         MOVE    JYO-F03     TO        DENKNM
         MOVE    SPACE       TO        ERRMSG
         MOVE   "ERRMSG"     TO        DSP-GRP
         PERFORM DSP-WR-SEC
         MOVE   "M"          TO        EDIT-OPTION  OF  DENKCD
         MOVE    SPACE       TO        EDIT-CURSOR  OF  DENKCD
         MOVE   "HEAD01"     TO        DSP-GRP
         PERFORM DSP-WR-SEC
     END-READ.
*  年度
***  前後２年算出
     COMPUTE  NENDO-BEFORE =  HEN-DATE-YYYY - 2.
     COMPUTE  NENDO-AFTER  =  HEN-DATE-YYYY + 2.
***  未入力
     IF ( NENDO   NOT NUMERIC )  OR
        ( NENDO   =   ZERO    )
         MOVE    MSG08     TO        ERRMSG
         MOVE   "ERRMSG"     TO        DSP-GRP
         PERFORM DSP-WR-SEC
         MOVE   "R"        TO        EDIT-OPTION  OF  NENDO
         MOVE   "C"        TO        EDIT-CURSOR  OF  NENDO
         MOVE   "HEAD01"   TO        DSP-GRP
         PERFORM DSP-WR-SEC
         GO                TO        MAIN-020
     ELSE
         MOVE   "M"        TO        EDIT-OPTION  OF  NENDO
         MOVE    SPACE     TO        EDIT-CURSOR  OF  NENDO
         MOVE   "HEAD01"   TO        DSP-GRP
         PERFORM DSP-WR-SEC
     END-IF.
***  許容年
     IF ( NENDO  <   NENDO-BEFORE ) OR
        ( NENDO  >   NENDO-AFTER  )
         MOVE    MSG08     TO        ERRMSG
         MOVE   "ERRMSG"     TO        DSP-GRP
         PERFORM DSP-WR-SEC
         MOVE   "R"        TO        EDIT-OPTION  OF  NENDO
         MOVE   "C"        TO        EDIT-CURSOR  OF  NENDO
         MOVE   "HEAD01"   TO        DSP-GRP
         PERFORM DSP-WR-SEC
         GO                TO        MAIN-020
     ELSE
         MOVE   "M"        TO        EDIT-OPTION  OF  NENDO
         MOVE    SPACE     TO        EDIT-CURSOR  OF  NENDO
         MOVE   "HEAD01"   TO        DSP-GRP
         PERFORM DSP-WR-SEC
     END-IF.
*  シーズン
     IF  SEASON  =  SPACE
         MOVE    MSG03       TO        ERRMSG
         MOVE   "ERRMSG"     TO        DSP-GRP
         PERFORM DSP-WR-SEC
         MOVE    SPACE       TO        SEASNM
         MOVE   "R"          TO        EDIT-OPTION  OF  SEASON
         MOVE   "C"          TO        EDIT-CURSOR  OF  SEASON
         MOVE   "HEAD01"     TO        DSP-GRP
         PERFORM DSP-WR-SEC
         GO                  TO        MAIN-020
     ELSE
         MOVE   "M"          TO        EDIT-OPTION  OF  SEASON
         MOVE    SPACE       TO        EDIT-CURSOR  OF  SEASON
         MOVE   "HEAD01"     TO        DSP-GRP
         PERFORM DSP-WR-SEC
     END-IF.
*
     MOVE        SEASON      TO        SEA-F01.
     READ        SEASONL1
       INVALID
         MOVE    MSG11       TO        ERRMSG
         MOVE   "ERRMSG"     TO        DSP-GRP
         PERFORM DSP-WR-SEC
         MOVE    SPACE       TO        SEASNM
         MOVE   "R"          TO        EDIT-OPTION  OF  SEASON
         MOVE   "C"          TO        EDIT-CURSOR  OF  SEASON
         MOVE   "HEAD01"     TO        DSP-GRP
         PERFORM DSP-WR-SEC
         GO                  TO        MAIN-020
       NOT INVALID
         MOVE    SEA-F02     TO        SEASNM
         MOVE    SPACE       TO        ERRMSG
         MOVE   "ERRMSG"     TO        DSP-GRP
         PERFORM DSP-WR-SEC
         MOVE   "M"          TO        EDIT-OPTION  OF  SEASON
         MOVE    SPACE       TO        EDIT-CURSOR  OF  SEASON
         MOVE   "HEAD01"     TO        DSP-GRP
         PERFORM DSP-WR-SEC
     END-READ.
*  倉庫
     IF  SOKCD   =  SPACE
         MOVE    MSG07       TO        ERRMSG
         MOVE   "ERRMSG"     TO        DSP-GRP
         PERFORM DSP-WR-SEC
         MOVE    SPACE       TO        SOKNM
         MOVE   "R"          TO        EDIT-OPTION  OF  SOKCD
         MOVE   "C"          TO        EDIT-CURSOR  OF  SOKCD
         MOVE   "HEAD01"     TO        DSP-GRP
         PERFORM DSP-WR-SEC
         GO                  TO        MAIN-020
     ELSE
         MOVE   "M"          TO        EDIT-OPTION  OF  SOKCD
         MOVE    SPACE       TO        EDIT-CURSOR  OF  SOKCD
         MOVE   "HEAD01"     TO        DSP-GRP
         PERFORM DSP-WR-SEC
     END-IF.
*
     MOVE        SOKCD       TO        SOK-F01.
     READ        ZSOKMS1
       INVALID
         MOVE    MSG09       TO        ERRMSG
         MOVE   "ERRMSG"     TO        DSP-GRP
         PERFORM DSP-WR-SEC
         MOVE    SPACE       TO        SOKNM
         MOVE   "R"          TO        EDIT-OPTION  OF  SOKCD
         MOVE   "C"          TO        EDIT-CURSOR  OF  SOKCD
         MOVE   "HEAD01"     TO        DSP-GRP
         PERFORM DSP-WR-SEC
         GO                  TO        MAIN-020
       NOT INVALID
         MOVE    SOK-F02     TO        SOKNM
         MOVE    SPACE       TO        ERRMSG
         MOVE   "ERRMSG"     TO        DSP-GRP
         PERFORM DSP-WR-SEC
         MOVE   "M"          TO        EDIT-OPTION  OF  SOKCD
         MOVE    SPACE       TO        EDIT-CURSOR  OF  SOKCD
         MOVE   "HEAD01"     TO        DSP-GRP
         PERFORM DSP-WR-SEC
     END-READ.
*  商品ＣＤ・品単ＣＤ
     IF  SKTCD   =  SPACE
         MOVE    MSG12       TO        ERRMSG
         MOVE   "ERRMSG"     TO        DSP-GRP
         PERFORM DSP-WR-SEC
         MOVE    SPACE       TO        SYONM1
         MOVE    SPACE       TO        SYONM2
         MOVE   "R"          TO        EDIT-OPTION  OF  SKTCD
         MOVE   "C"          TO        EDIT-CURSOR  OF  SKTCD
         MOVE   "HEAD01"     TO        DSP-GRP
         PERFORM DSP-WR-SEC
         GO                  TO        MAIN-020
     ELSE
         MOVE   "M"          TO        EDIT-OPTION  OF  SKTCD
         MOVE    SPACE       TO        EDIT-CURSOR  OF  SKTCD
         MOVE   "HEAD01"     TO        DSP-GRP
         PERFORM DSP-WR-SEC
     END-IF.
*
*振替情報ファイル存在チェック
     MOVE    DENKCD            TO        HED-F01.
     MOVE    NENDO             TO        HED-F02.
     MOVE    SEASON            TO        HED-F03.
     MOVE    SOKCD             TO        HED-F04.
     MOVE    SKTCD             TO        HED-F05.
     MOVE    HINT1             TO        HED-F06.
     MOVE    HINT2             TO        HED-F07.
     MOVE    HINT3             TO        HED-F08.
     READ    SFRHEDL1
       INVALID
             MOVE    MSG01     TO        ERRMSG
             MOVE    1         TO        INV-SW
             MOVE   "ERRMSG"   TO        DSP-GRP
             PERFORM DSP-WR-SEC
             MOVE    SPACE     TO        SYONM1
             MOVE    SPACE     TO        SYONM2
             MOVE   "R"        TO        EDIT-OPTION  OF  DENKCD
                                         EDIT-OPTION  OF  NENDO
                                         EDIT-OPTION  OF  SEASON
                                         EDIT-OPTION  OF  SOKCD
                                         EDIT-OPTION  OF  SKTCD
                                         EDIT-OPTION  OF  HINT1
                                         EDIT-OPTION  OF  HINT2
                                         EDIT-OPTION  OF  HINT3
             MOVE   "C"        TO        EDIT-CURSOR  OF  DENKCD
             MOVE   "HEAD01"   TO        DSP-GRP
             PERFORM DSP-WR-SEC
             GO                TO        MAIN-020
       NOT INVALID
             MOVE   "M"        TO        EDIT-OPTION  OF  DENKCD
                                         EDIT-OPTION  OF  NENDO
                                         EDIT-OPTION  OF  SEASON
                                         EDIT-OPTION  OF  SOKCD
                                         EDIT-OPTION  OF  SKTCD
                                         EDIT-OPTION  OF  HINT1
                                         EDIT-OPTION  OF  HINT2
                                         EDIT-OPTION  OF  HINT3
             MOVE   "HEAD01"   TO        DSP-GRP
             PERFORM DSP-WR-SEC
     END-READ.
*
*明細初期内容表示（FORM振替ファイル）
*            　　　　　　　　　　商品名
     MOVE    HED-F09     TO        SYONM1.
     MOVE    HED-F10     TO        SYONM2.
*            　　　　　　　　　　仕入先
     MOVE    HED-F12     TO        SIRCD  SHI-F01.
     READ    ZSHIMS1
       INVALID
             MOVE    ALL NC"＊"   TO   SIRNM
       NOT INVALID
             MOVE    SHI-F02      TO   SIRNM
     END-READ.
*            　　　　　　　　　　ＪＡＮＣＤ
     MOVE    HED-F13     TO        JANCD.
*            　　　　　　　　　　入荷予定日
     MOVE    HED-F14     TO        YOTDAT.
*
     IF   SYORI  =  1
*                 　　　　　　　　　　棚番
          MOVE    HED-F11     TO        TANBAN
*                 　　　　　　　　　　発注数／入荷数
          MOVE    ZERO        TO        HNSU
*                 　　　　　　　　　　ＭＳＧ
          MOVE    HED-F23     TO        HMESSG
     END-IF.
*            　　　　　　　　　　発注数合計
     MOVE    HED-F15          TO   HACGK.
*            　　　　　　　　　　最終発注日
     IF    ( HED-F18  NOT = ZERO  ) AND
           ( HED-F18  IS  NUMERIC )
             MOVE    HED-F18(1:4)     TO   HACDAT(1:4)
             MOVE    "/"              TO   HACDAT(5:1)
             MOVE    HED-F18(5:2)     TO   HACDAT(6:2)
             MOVE    "/"              TO   HACDAT(8:1)
             MOVE    HED-F18(7:2)     TO   HACDAT(9:2)
     ELSE
             MOVE    SPACE            TO   HACDAT
     END-IF.
*            　　　　　　　　　　入荷数合計
     MOVE    HED-F19          TO   NYKGK.
*            　　　　　　　　　　最終入荷日
     IF    ( HED-F22  NOT = ZERO  ) AND
           ( HED-F22  IS  NUMERIC )
             MOVE    HED-F22(1:4)     TO   NYKDAT(1:4)
             MOVE    "/"              TO   NYKDAT(5:1)
             MOVE    HED-F22(5:2)     TO   NYKDAT(6:2)
             MOVE    "/"              TO   NYKDAT(8:1)
             MOVE    HED-F22(7:2)     TO   NYKDAT(9:2)
     ELSE
             MOVE    SPACE            TO   NYKDAT
     END-IF.
*
     MOVE       "MEISAI"     TO        DSP-GRP.
     PERFORM     DSP-WR-SEC.
*            　　　　　　　　　　完納情報
     MOVE    HED-F24     TO        KANKBN  BK-KANKBN.
     MOVE    HED-F25     TO                BK-KANDAT.
     MOVE    HED-F26     TO                BK-KANBMN.
     MOVE    HED-F27     TO                BK-KANTAN.
     IF      HED-F24     =         "1"
             MOVE    HED-F26  TO   KANTAN(1:4) TAN-F01
             MOVE    "-"      TO   KANTAN(5:1)
             MOVE    HED-F27  TO   KANTAN(6:2) TAN-F02
             READ    TANMS1
               INVALID
                 MOVE    ALL NC"＊"   TO   KTANNM
               NOT INVALID
                 MOVE    TAN-F03      TO   KTANNM
             END-READ
             MOVE    HED-F25(1:4)     TO   KANDAT(1:4)
             MOVE    "/"              TO   KANDAT(5:1)
             MOVE    HED-F25(5:2)     TO   KANDAT(6:2)
             MOVE    "/"              TO   KANDAT(8:1)
             MOVE    HED-F25(7:2)     TO   KANDAT(9:2)
     ELSE
             MOVE    SPACE            TO   KANTAN
                                           KTANNM
                                           KANDAT
     END-IF.
*T
*    DISPLAY "SOKCD  =" SOKCD   UPON CONS.
*    DISPLAY "HED-F24=" HED-F24 UPON CONS.
*    DISPLAY "KANTAN =" KANTAN  UPON CONS.
*    DISPLAY "KTANNM =" KTANNM  UPON CONS.
*    DISPLAY "KANDAT =" KANDAT  UPON CONS.
*T           　　　　　　　　　　完納情報
*
     MOVE       "KANNOU"     TO        DSP-GRP.
     PERFORM     DSP-WR-SEC.
*
*↓2017/04/18
     IF      HED-F24     =         "1"
             MOVE    MSG23     TO        ERRMSG
             MOVE   "ERRMSG"   TO        DSP-GRP
             PERFORM DSP-WR-SEC
             MOVE   "R"        TO        EDIT-OPTION  OF  DENKCD
                                         EDIT-OPTION  OF  NENDO
                                         EDIT-OPTION  OF  SEASON
                                         EDIT-OPTION  OF  SOKCD
                                         EDIT-OPTION  OF  SKTCD
                                         EDIT-OPTION  OF  HINT1
                                         EDIT-OPTION  OF  HINT2
                                         EDIT-OPTION  OF  HINT3
             MOVE   "C"        TO        EDIT-CURSOR  OF  DENKCD
             MOVE   "HEAD01"   TO        DSP-GRP
             PERFORM DSP-WR-SEC
             IF      PARA-CALL  =        "P"
                     GO        TO        MAIN-010
             ELSE
                     GO        TO        MAIN-020
             END-IF
     END-IF.
*↑2017/04/18
*
     IF    (PARA-CALL  =  "C")
            MOVE PMSG03 TO        PFMSG
            MOVE       "PFMSG"    TO        DSP-GRP
            PERFORM     DSP-WR-SEC
     END-IF.
*
 MAIN-022.
*発注日／入荷日入力
     MOVE       "HNDATE"     TO        DSP-GRP.
     PERFORM     DSP-RD-SEC.
     EVALUATE    DSP-FNC
       WHEN
        "F004"
           MOVE  "1"         TO        PSW
           MOVE  "Y"         TO        PF4-SEL
           GO                TO        MAIN-SEC
       WHEN
        "F009"
           GO                TO        MAIN-020
       WHEN
        "E000"
           CONTINUE
       WHEN
         OTHER
           MOVE    MSG18     TO        ERRMSG
           MOVE   "ERRMSG"   TO        DSP-GRP
           PERFORM DSP-WR-SEC
           GO                TO        MAIN-022
     END-EVALUATE.
     PERFORM     DSP-WR-SEC.
 MAIN-023.
*発注日／入荷日チェック
     IF ( HNDATE  NOT NUMERIC ) OR
        ( HNDATE   =  ZERO    )
         MOVE     "R"        TO   EDIT-OPTION  OF  HNDATE
         MOVE     "C"        TO   EDIT-CURSOR  OF  HNDATE
         IF        PARA-SYUBETU   =  "1"
                   MOVE      MSG19     TO   ERRMSG
         ELSE
                   MOVE      MSG20     TO   ERRMSG
         END-IF
         MOVE     "ERRMSG"     TO   DSP-GRP
         PERFORM   DSP-WR-SEC
         MOVE     "HNDATE"   TO   DSP-GRP
         PERFORM   DSP-WR-SEC
         GO   TO   MAIN-022
     ELSE
***  論理チェック
         MOVE     "2"            TO   LINK-IN-KBN
         MOVE     ZERO           TO   LINK-IN-YMD6
         MOVE     HNDATE         TO   LINK-IN-YMD8
         MOVE     ZERO           TO   LINK-OUT-RET
         MOVE     ZERO           TO   LINK-OUT-YMD
         CALL     "SKYDTCKB"     USING   LINK-IN-KBN
                                         LINK-IN-YMD6
                                         LINK-IN-YMD8
                                         LINK-OUT-RET
                                         LINK-OUT-YMD
         IF   LINK-OUT-RET   = 9
              MOVE     "R"        TO   EDIT-OPTION  OF  HNDATE
              MOVE     "C"        TO   EDIT-CURSOR  OF  HNDATE
              MOVE      MSG17     TO   ERRMSG
              MOVE     "ERRMSG"     TO   DSP-GRP
              PERFORM   DSP-WR-SEC
              MOVE     "HNDATE"   TO   DSP-GRP
              PERFORM   DSP-WR-SEC
              GO   TO   MAIN-022
         END-IF
*
         MOVE      SPACE     TO   ERRMSG
         MOVE     "ERRMSG"   TO   DSP-GRP
         PERFORM   DSP-WR-SEC
         MOVE     "M"        TO   EDIT-OPTION  OF  HNDATE
         MOVE      SPACE     TO   EDIT-CURSOR  OF  HNDATE
         MOVE     "HNDATE"   TO   DSP-GRP
         PERFORM   DSP-WR-SEC
     END-IF.
 MAIN-024.
*振替明細ファイル存在チェック
     MOVE    DENKCD            TO        MEI-F01.
     MOVE    NENDO             TO        MEI-F02.
     MOVE    SEASON            TO        MEI-F03.
     MOVE    SOKCD             TO        MEI-F04.
     MOVE    SKTCD             TO        MEI-F05.
     MOVE    HINT1             TO        MEI-F06.
     MOVE    HINT2             TO        MEI-F07.
     MOVE    HINT3             TO        MEI-F08.
     MOVE    PARA-SYUBETU      TO        MEI-F11.
     MOVE    HNDATE            TO        MEI-F12.
     READ    SFRMEIL1
       INVALID
         IF ( SYORI = 2 ) OR ( SYORI = 3 )
             MOVE    MSG01     TO        ERRMSG
             MOVE    1         TO        INV-SW
             MOVE   "ERRMSG"   TO        DSP-GRP
             PERFORM DSP-WR-SEC
             MOVE   "R"        TO        EDIT-OPTION  OF  HNDATE
             MOVE   "C"        TO        EDIT-CURSOR  OF  HNDATE
             MOVE   "HNDATE"   TO        DSP-GRP
             PERFORM DSP-WR-SEC
             GO                TO        MAIN-022
         ELSE
             MOVE   "M"        TO        EDIT-OPTION  OF  HNDATE
             MOVE   "HNDATE"   TO        DSP-GRP
             PERFORM DSP-WR-SEC
         END-IF
       NOT INVALID
         IF ( SYORI = 1 )
             MOVE    MSG02     TO        ERRMSG
             MOVE    1         TO        INV-SW
             MOVE   "ERRMSG"   TO        DSP-GRP
             PERFORM DSP-WR-SEC
             MOVE   "R"        TO        EDIT-OPTION  OF  HNDATE
             MOVE   "C"        TO        EDIT-CURSOR  OF  HNDATE
             MOVE   "HNDATE"   TO        DSP-GRP
             PERFORM DSP-WR-SEC
             GO                TO        MAIN-022
         ELSE
             MOVE   "M"        TO        EDIT-OPTION  OF  HNDATE
             MOVE   "HNDATE"   TO        DSP-GRP
             PERFORM DSP-WR-SEC
         END-IF
     END-READ.
*
*明細初期内容表示（FORM明細ファイル）
 MAIN-029.
*
     IF   SYORI  =  1
          CONTINUE
     ELSE
*                 　　　　　　　　　　棚番
          MOVE    MEI-F13     TO        TANBAN
**********#2017/04/14 NAV ST
          MOVE    MEI-F13     TO        WK-BAK-TANBAN
**********#2017/04/14 NAV ED
*                 　　　　　　　　　　発注数／入荷数
          MOVE    MEI-F14     TO        HNSU
**********#2017/04/14 NAV ST
          MOVE    MEI-F14     TO        WK-BAK-HNSU
**********#2017/04/14 NAV ED
          MOVE    MEI-F12(1:6) TO       WK-BAK-HNDATE
*                 　　　　　　　　　　ＭＳＧ
          MOVE    MEI-F15     TO        HMESSG
     END-IF.
*
     MOVE       "MEISAI"     TO        DSP-GRP.
     PERFORM     DSP-WR-SEC.
*
     IF  (SYORI  =  3)
         GO                  TO        MAIN-040
     END-IF.
*
 MAIN-030.
     MOVE        PMSG03      TO        PFMSG.
     MOVE       "PFMSG"      TO        DSP-GRP.
     PERFORM     DSP-WR-SEC.
*
*明細項目 入力
     MOVE       "MEISAI"   TO        DSP-GRP.
     PERFORM     DSP-RD-SEC.
     EVALUATE      DSP-FNC
         WHEN      "F004"
                    MOVE  "1"      TO  PSW
                    MOVE  "Y"      TO  PF4-SEL
                    GO             TO  MAIN-SEC
         WHEN      "F009"
                    GO             TO  MAIN-022
         WHEN      "E000"
                             CONTINUE
         WHEN      OTHER
                             MOVE    MSG18     TO   ERRMSG
                             MOVE   "ERRMSG"   TO   DSP-GRP
                             PERFORM DSP-WR-SEC
                             GO        TO   MAIN-030
     END-EVALUATE.
*
     MOVE     SPACE          TO   ERRMSG.
     MOVE     "ERRMSG"       TO   DSP-GRP.
     PERFORM  DSP-WR-SEC.
     MOVE       "MEISAI"   TO        DSP-GRP.
     PERFORM  DSP-WR-SEC.
*項目入力チェック
*　棚番:ノーチェック
*　発注数／入荷数
     IF ( HNSU    NOT NUMERIC ) OR
        ( HNSU     =  ZERO    )
         MOVE     "R"        TO   EDIT-OPTION  OF  HNSU
         MOVE     "C"        TO   EDIT-CURSOR  OF  HNSU
         IF        PARA-SYUBETU  =  "1"
                   MOVE      MSG21     TO   ERRMSG
         ELSE
                   MOVE      MSG22     TO   ERRMSG
         END-IF
         MOVE     "ERRMSG"     TO   DSP-GRP
         PERFORM   DSP-WR-SEC
         MOVE     "MEISAI"   TO   DSP-GRP
         PERFORM   DSP-WR-SEC
         GO   TO   MAIN-030
     ELSE
         MOVE     "M"        TO   EDIT-OPTION  OF  HNSU
         MOVE      SPACE     TO   EDIT-CURSOR  OF  HNSU
         MOVE     "MEISAI"   TO   DSP-GRP
         PERFORM   DSP-WR-SEC
     END-IF.
*  ＭＳＧ：ノーチェック
*
 MAIN-031.
*完納区分入力
*  入荷時のみ
     IF          PARA-SYUBETU   =  "2"
                 CONTINUE
     ELSE
                 GO        TO          MAIN-040
     END-IF.
*
     MOVE       "KANKBN"   TO          DSP-GRP.
     PERFORM     DSP-RD-SEC.
     EVALUATE      DSP-FNC
         WHEN      "F004"
                    MOVE  "1"      TO  PSW
                    MOVE  "Y"      TO  PF4-SEL
                    GO             TO  MAIN-SEC
         WHEN      "F009"
                    GO             TO  MAIN-030
         WHEN      "E000"
                             CONTINUE
         WHEN      OTHER
                             MOVE    MSG18     TO   ERRMSG
                             MOVE   "ERRMSG"   TO   DSP-GRP
                             PERFORM DSP-WR-SEC
                             GO        TO   MAIN-031
     END-EVALUATE.
*
     MOVE     SPACE          TO   ERRMSG.
     MOVE     "ERRMSG"       TO   DSP-GRP.
     PERFORM  DSP-WR-SEC.
     MOVE       "KANNOU"     TO   DSP-GRP.
     PERFORM  DSP-WR-SEC.
*
*完納区分チェック
     IF ( KANKBN  NOT = " "   ) AND
        ( KANKBN  NOT = "1"   )
         MOVE     "R"        TO   EDIT-OPTION  OF  KANKBN
         MOVE     "C"        TO   EDIT-CURSOR  OF  KANKBN
         MOVE      MSG16     TO   ERRMSG
         MOVE     "ERRMSG"   TO   DSP-GRP
         PERFORM   DSP-WR-SEC
         MOVE     "KANNOU"   TO   DSP-GRP
         PERFORM   DSP-WR-SEC
         GO   TO   MAIN-031
     END-IF.
     IF ( KANKBN   =    " "  )
         MOVE      SPACE     TO   KANTAN
                                  KTANNM
                                  KANDAT
     END-IF.
     IF ( KANKBN   =    "1"  )
          IF ( BK-KANKBN   =    "1"  )
*--            CONTINUE
               MOVE  BK-KANBMN       TO KANTAN(1:4)
               MOVE  "-"             TO KANTAN(5:1)
               MOVE  BK-KANTAN       TO KANTAN(6:2)
               MOVE  TAN-F03         TO KTANNM
               MOVE  BK-KANDAT(1:4)  TO KANDAT(1:4)
               MOVE  "/"             TO KANDAT(5:1)
               MOVE  BK-KANDAT(5:2)  TO KANDAT(6:2)
               MOVE  "/"             TO KANDAT(8:1)
               MOVE  BK-KANDAT(7:2)  TO KANDAT(9:2)
          ELSE
               MOVE  PARA-BUMONCD TO KANTAN(1:4)
               MOVE  "-"          TO KANTAN(5:1)
               MOVE  PARA-TANCD   TO KANTAN(6:2)
               MOVE  TAN-F03      TO KTANNM
               MOVE  HEN-DATE     TO KANDAT
     END-IF.
     MOVE     "M"        TO   EDIT-OPTION  OF  KANKBN.
     MOVE      SPACE     TO   EDIT-CURSOR  OF  KANKBN.
     MOVE     "KANNOU"   TO   DSP-GRP.
     PERFORM   DSP-WR-SEC.
*
 MAIN-040.
*確認入力
     MOVE     "Y"            TO   ANS.
     MOVE     "ANS"          TO   DSP-GRP.
     PERFORM   DSP-WR-SEC.
*
     IF        SYORI   =   1
               MOVE      PMSG06        TO   PFMSG
     END-IF.
     IF        SYORI   =   2
               MOVE      PMSG06        TO   PFMSG
     END-IF.
     IF        SYORI   =   3
               MOVE      PMSG06        TO   PFMSG
     END-IF.
     MOVE     "PFMSG"        TO   DSP-GRP.
     PERFORM   DSP-WR-SEC.
     MOVE     "ANS"          TO   DSP-GRP.
     PERFORM   DSP-RD-SEC.
     EVALUATE  DSP-FNC
         WHEN      "F001"
*          社内振替情報照会（明細）ＣＡＬＬ
           IF ( SYORI  = 1 OR 2 OR 3 )
                MOVE     DENKCD   TO        SFU3070I-DENKU
                MOVE     NENDO    TO        SFU3070I-NENDO
                MOVE     SEASON   TO        SFU3070I-SEASON
                MOVE     SOKCD    TO        SFU3070I-SOUKO
                MOVE     SKTCD    TO        SFU3070I-SHOHIN
                MOVE     HINT1    TO        SFU3070I-HINTAN1
                MOVE     HINT2    TO        SFU3070I-HINTAN2
                MOVE     HINT3    TO        SFU3070I-HINTAN3
                MOVE     SIRCD    TO        SFU3070I-SHIIRE
                MOVE     HACGK    TO        SFU3070I-HACKEI
                MOVE     NYKGK    TO        SFU3070I-NYUKEI
                CALL    "SFU3070I"  USING   SFU3070I-DENKU
                                            SFU3070I-NENDO
                                            SFU3070I-SEASON
                                            SFU3070I-SOUKO
                                            SFU3070I-SHOHIN
                                            SFU3070I-HINTAN1
                                            SFU3070I-HINTAN2
                                            SFU3070I-HINTAN3
                                            SFU3070I-SHIIRE
                                            SFU3070I-HACKEI
                                            SFU3070I-NYUKEI
*   　          元画面に戻る
                MOVE   "CL"            TO      DSP-PRO
                MOVE   "SCREEN"        TO      DSP-GRP
                MOVE    TAIHI-PGID     TO      PGID
                MOVE    TAIHI-FORMID   TO      FORMID
                MOVE    TAIHI-HACNYK   TO      HACNYK
                MOVE    TAIHI-SDATE    TO      SDATE
                MOVE    TAIHI-STIME    TO      STIME
                MOVE    TAIHI-MTANTO   TO      MTANTO
                MOVE   "FFU30501"      TO      DSP-FMT
                WRITE   FFU30501
                MOVE    SPACE          TO      DSP-PRO
                GO                     TO      MAIN-040
           ELSE
                MOVE    MSG18     TO   ERRMSG
                MOVE   "ERRMSG"   TO   DSP-GRP
                PERFORM DSP-WR-SEC
                GO   TO   MAIN-040
           END-IF
         WHEN      "F004"
           MOVE     SPACE    TO        ANS
           MOVE    "1"       TO        PSW
           MOVE    "Y"       TO        PF4-SEL
           GO                TO        MAIN-SEC
         WHEN      "F009"
           IF  (SYORI  = 1 OR 2)
                             GO        TO   MAIN-030
           ELSE
                             GO        TO   MAIN-022
           END-IF
         WHEN      "E000"
           CONTINUE
         WHEN      OTHER
                             MOVE    MSG18     TO   ERRMSG
                             MOVE   "ERRMSG"   TO   DSP-GRP
                             PERFORM DSP-WR-SEC
                             GO   TO   MAIN-040
     END-EVALUATE.
*    PERFORM     DSP-WR-SEC.
*
     IF  (ANS  NOT =  "Y")
           MOVE    MSG06       TO   ERRMSG
           MOVE   "ERRMSG"     TO   DSP-GRP
           PERFORM DSP-WR-SEC
           GO          TO   MAIN-040
     ELSE
           MOVE    SPACE       TO   ERRMSG
           MOVE   "ERRMSG"     TO   DSP-GRP
           PERFORM DSP-WR-SEC
     END-IF.
*
     IF  (SYORI  =  3)
           GO          TO   MAIN-080
     END-IF.
*
 MAIN-080.
*明細ファイル更新
     PERFORM  SFRMEIL1-SET-SEC.
     PERFORM  SFRMEIL1-UPDT-SEC.
*
 MAIN-090.
*振分情報ファイル更新
     PERFORM  SFRMEIL1-SUM-SEC.
     PERFORM  SFRHEDL1-UPDT-SEC.
*
*初期画面へ
     MOVE    SYORI          TO      TAIHI-SYORI.
     MOVE    SYKNM          TO      TAIHI-SYKNM.
     MOVE    DENKCD         TO      TAIHI-DENKCD.
     MOVE    DENKNM         TO      TAIHI-DENKNM.
     MOVE    NENDO          TO      TAIHI-NENDO.
     MOVE    SEASON         TO      TAIHI-SEASON.
     MOVE    SEASNM         TO      TAIHI-SEASNM.
     MOVE    SOKCD          TO      TAIHI-SOKCD.
     MOVE    SOKNM          TO      TAIHI-SOKNM.
     MOVE    SKTCD          TO      TAIHI-SKTCD.
     MOVE    HINT1          TO      TAIHI-HINT1.
     MOVE    HINT2          TO      TAIHI-HINT2.
     MOVE    HINT3          TO      TAIHI-HINT3.
     MOVE    MSG1           TO      TAIHI-MSG1.
     MOVE    MSG2           TO      TAIHI-MSG2.
     MOVE    MSG3           TO      TAIHI-MSG3.
     MOVE    SPACE          TO      FFU30501.
     MOVE    TAIHI-PGID     TO      PGID.
     MOVE    TAIHI-FORMID   TO      FORMID.
     MOVE    TAIHI-SDATE    TO      SDATE.
     MOVE    TAIHI-HACNYK   TO      HACNYK.
     MOVE    TAIHI-STIME    TO      STIME.
     MOVE    TAIHI-SYORI    TO      SYORI.
     MOVE    TAIHI-SYKNM    TO      SYKNM.
     MOVE    TAIHI-MTANTO   TO      MTANTO.
     MOVE    TAIHI-TANNM    TO      TANNM.
     MOVE    TAIHI-DENKCD   TO      DENKCD.
     MOVE    TAIHI-DENKNM   TO      DENKNM.
     MOVE    TAIHI-NENDO    TO      NENDO.
     MOVE    TAIHI-SEASON   TO      SEASON.
     MOVE    TAIHI-SEASNM   TO      SEASNM.
     MOVE    TAIHI-SOKCD    TO      SOKCD.
     MOVE    TAIHI-SOKNM    TO      SOKNM.
     MOVE    TAIHI-SKTCD    TO      SKTCD.
     MOVE    TAIHI-HINT1    TO      HINT1.
     MOVE    TAIHI-HINT2    TO      HINT2.
     MOVE    TAIHI-HINT3    TO      HINT3.
     MOVE    TAIHI-MSG1     TO      MSG1.
     MOVE    TAIHI-MSG2     TO      MSG2.
     MOVE    TAIHI-MSG3     TO      MSG3.
     MOVE   "SCREEN"        TO      DSP-GRP.
     PERFORM DSP-WR-SEC.
*
     GO             TO      MAIN-020.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*                   ＥＮＤ処理                                 *
****************************************************************
 END-SEC               SECTION.
     CLOSE             SFRHEDL1  SFRMEIL1  ZSOKMS1  TANMS1
                       JYOKEN1   SEASONL1  ZSHIMS1
                       DSPFILE.
*#2017/04/14 NAV ST
     CLOSE             ZAMZAIL1  MEIMS1.
*#2017/04/14 NAV ED
 END-EXIT.
     EXIT.
****************************************************************
*    画面　　ＲＥＡＤ　　                                      *
****************************************************************
 DSP-RD-SEC             SECTION.
     MOVE  "NE"               TO  DSP-PRO.
     READ                         DSPFILE.
 DSP-RD-EXIT.
     EXIT.
****************************************************************
*    画面    ＷＲＩＴＥ                                      *
****************************************************************
 DSP-WR-SEC             SECTION.
     MOVE       SPACE        TO  DSP-PRO.
     WRITE      FFU30501.
 DSP-WR-EXIT.
     EXIT.
****************************************************************
*    初期画面　表示                                          *
****************************************************************
 DSP-INIT-SEC             SECTION.
     MOVE     SPACE           TO   FFU30501.
     MOVE     SPACE           TO   DSP-AREA.
     MOVE    "FFU30501"       TO   DSP-FMT.
     MOVE    "SCREEN"         TO   DSP-GRP.
     MOVE    "CL"             TO   DSP-PRO.
*
     MOVE     HEN-DATE        TO   SDATE.
     MOVE     HEN-TIME        TO   STIME.
     MOVE    "SFU3050I"       TO   PGID.
     MOVE    "FFU30501"       TO   FORMID.
     MOVE     PARA-BUMONCD    TO   MTANTO(1:4)  TAN-F01.
     MOVE    "-"              TO   MTANTO(5:1).
     MOVE     PARA-TANCD      TO   MTANTO(6:2)  TAN-F02.
     MOVE     SPACE           TO   BK-KANKBN.
     READ     TANMS1
        INVALID
              MOVE    ALL NC"＊"   TO   TAN-F03
     END-READ
     MOVE     TAN-F03        TO      TANNM.
*
     EVALUATE PARA-SYUBETU
       WHEN   "1"
              MOVE  NC"　発注　"     TO    HACNYK
              MOVE  NC"発注日　"     TO    MSG2
              MOVE  NC"発注情報"     TO    MSG1
              MOVE  NC"発注数　"     TO    MSG3
       WHEN   "2"
              MOVE  NC"　入荷　"     TO    HACNYK
              MOVE  NC"入荷日　"     TO    MSG2
              MOVE  NC"入荷情報"     TO    MSG1
              MOVE  NC"入荷数　"     TO    MSG3
       WHEN   OTHER
              MOVE  NC"　？？　"     TO    HACNYK
              MOVE  NC"？？？？"     TO    MSG1 MSG2 MSG3
     END-EVALUATE.
*
     IF       PARA-SYORI     NOT =   " "
              MOVE     PARA-SYORI    TO      SYORI
     END-IF.
     EVALUATE PARA-SYORI
       WHEN   "1"
              MOVE  NC"登録"         TO    SYKNM
       WHEN   "2"
              MOVE  NC"修正"         TO    SYKNM
       WHEN   "3"
              MOVE  NC"削除"         TO    SYKNM
       WHEN   OTHER
              MOVE  NC"　　"         TO    SYKNM
     END-EVALUATE.
*
     IF    (  PARA-CALL  =   "P" ) AND
           (  PF4-SEL    =   " " )
              MOVE     PARA-DENKU    TO      DENKCD JYO-F02
              MOVE     1             TO      JYO-F01
              READ     JYOKEN1
                 INVALID
                       MOVE  ALL NC"？"  TO  JYO-F03
              END-READ
              MOVE     JYO-F03       TO      DENKNM
              MOVE     PARA-NENDO    TO      NENDO
              MOVE     PARA-SEASON   TO      SEASON SEA-F01
              READ     SEASONL1
                 INVALID
                       MOVE  ALL NC"？"  TO  SEA-F02
              END-READ
              MOVE     SEA-F02       TO      SEASNM
              MOVE     PARA-SOUKO    TO      SOKCD SOK-F01
              READ     ZSOKMS1
                 INVALID
                       MOVE  ALL NC"？"  TO  SOK-F02
              END-READ
              MOVE     SOK-F02       TO      SOKNM
              MOVE     PARA-SHOHIN   TO      SKTCD
              MOVE     PARA-HINTAN1  TO      HINT1
              MOVE     PARA-HINTAN2  TO      HINT2
              MOVE     PARA-HINTAN3  TO      HINT3
     END-IF.
*
     IF    (  PARA-CALL  =   "P" ) AND
           (  PF4-SEL    =   "Y" )
              MOVE     TAIHI-DENKCD  TO      DENKCD
              MOVE     TAIHI-DENKNM  TO      DENKNM
              MOVE     TAIHI-NENDO   TO      NENDO
              MOVE     TAIHI-SEASON  TO      SEASON
              MOVE     TAIHI-SEASNM  TO      SEASNM
              MOVE     TAIHI-SOKCD   TO      SOKCD
              MOVE     TAIHI-SOKNM   TO      SOKNM
              MOVE     TAIHI-SKTCD   TO      SKTCD
              MOVE     TAIHI-HINT1   TO      HINT1
              MOVE     TAIHI-HINT2   TO      HINT2
              MOVE     TAIHI-HINT3   TO      HINT3
     END-IF.
*
     MOVE     PGID           TO      TAIHI-PGID.
     MOVE     FORMID         TO      TAIHI-FORMID.
     MOVE     SDATE          TO      TAIHI-SDATE.
     MOVE     STIME          TO      TAIHI-STIME.
     MOVE     MTANTO         TO      TAIHI-MTANTO.
     MOVE     TANNM          TO      TAIHI-TANNM.
     MOVE     HACNYK         TO      TAIHI-HACNYK.
     MOVE     SYORI          TO      TAIHI-SYORI.
     MOVE     SYKNM          TO      TAIHI-SYKNM.
     MOVE     DENKCD         TO      TAIHI-DENKCD.
     MOVE     DENKNM         TO      TAIHI-DENKNM.
     MOVE     NENDO          TO      TAIHI-NENDO.
     MOVE     SEASON         TO      TAIHI-SEASON.
     MOVE     SEASNM         TO      TAIHI-SEASNM.
     MOVE     SOKCD          TO      TAIHI-SOKCD.
     MOVE     SOKNM          TO      TAIHI-SOKNM.
     MOVE     SKTCD          TO      TAIHI-SKTCD.
     MOVE     HINT1          TO      TAIHI-HINT1.
     MOVE     HINT2          TO      TAIHI-HINT2.
     MOVE     HINT3          TO      TAIHI-HINT3.
*
     WRITE    FFU30501.
     IF       PARA-CALL  =  "C"
              INITIALIZE              FFU30501
     END-IF.
*
 DSP-INIT-EXIT.
     EXIT.
*
****************************************************************
*            画面よりレコードに転送                            *
****************************************************************
 SFRMEIL1-SET-SEC           SECTION.
     MOVE     DENKCD         TO        MEI-F01.
     MOVE     NENDO          TO        MEI-F02.
     MOVE     SEASON         TO        MEI-F03.
     MOVE     SOKCD          TO        MEI-F04.
     MOVE     SKTCD          TO        MEI-F05.
     MOVE     HINT1          TO        MEI-F06.
     MOVE     HINT2          TO        MEI-F07.
     MOVE     HINT3          TO        MEI-F08.
     MOVE     SYONM1         TO        MEI-F09.
     MOVE     SYONM2         TO        MEI-F10.
     MOVE     PARA-SYUBETU   TO        MEI-F11.
     MOVE     HNDATE         TO        MEI-F12.
     MOVE     TANBAN         TO        MEI-F13.
     MOVE     HNSU           TO        MEI-F14.
     MOVE     HMESSG         TO        MEI-F15.
     MOVE     SPACE          TO        MEI-F91.
     IF       SYORI     =    "1"
              MOVE  PARA-BUMONCD  TO   MEI-F92
              MOVE  PARA-TANCD    TO   MEI-F93
              MOVE  SYS-DATE      TO   MEI-F94
              MOVE  WK-TIME(1:6)  TO   MEI-F95
              MOVE  PARA-BUMONCD  TO   MEI-F96
              MOVE  PARA-TANCD    TO   MEI-F97
              MOVE  SYS-DATE      TO   MEI-F98
              MOVE  WK-TIME(1:6)  TO   MEI-F99
     END-IF.
     IF       SYORI     =    "2"
              MOVE  PARA-BUMONCD  TO   MEI-F96
              MOVE  PARA-TANCD    TO   MEI-F97
              MOVE  SYS-DATE      TO   MEI-F98
              MOVE  WK-TIME(1:6)  TO   MEI-F99
     END-IF.
*
 SFRMEIL1-SET-EXIT.
     EXIT.
****************************************************************
*  振替明細ファイル更新
****************************************************************
 SFRMEIL1-UPDT-SEC      SECTION.
*
*T↓
*    DISPLAY "SYORI=" SYORI UPON CONS.
*T↑
     EVALUATE   SYORI
         WHEN     1
**************#2017/04/14 NAV ST
**************在庫プラス
***********IF PARA-SYUBETU = "2"
              MOVE     HNSU      TO  WK-SURYO
              MOVE     SOKCD     TO  WK-KEY-SOKCD
              MOVE     SKTCD     TO  WK-KEY-SYOCD
              MOVE     HINT1     TO  WK-KEY-HINCD(1:5)
              MOVE     HINT2     TO  WK-KEY-HINCD(6:2)
              MOVE     HINT3     TO  WK-KEY-HINCD(8:1)
              MOVE     TANBAN    TO  WK-KEY-TANBAN
              MOVE     HNSU      TO  WK-SURYO
**************#2018/06/15 NAV ST
              MOVE     HNDATE(1:6) TO WRK-DATE2
**************#2018/06/15 NAV ED
              PERFORM  ZAIKO-SEC
***********END-IF
**************#2017/04/14 NAV ED
              MOVE     MEI-REC    TO  DATA-TAIHI
              WRITE    MEI-REC
              END-WRITE
         WHEN     2
**************#2017/04/14 NAV ST
**************在庫マイナス
***********IF PARA-SYUBETU = "2"
              MOVE     HNSU     TO  WK-SURYO
              MOVE     SOKCD    TO  WK-KEY-SOKCD
              MOVE     SKTCD    TO  WK-KEY-SYOCD
              MOVE     HINT1    TO  WK-KEY-HINCD(1:5)
              MOVE     HINT2    TO  WK-KEY-HINCD(6:2)
              MOVE     HINT3    TO  WK-KEY-HINCD(8:1)
              MOVE WK-BAK-TANBAN TO WK-KEY-TANBAN
              MOVE WK-BAK-HNSU   TO WK-SURYO
              COMPUTE WK-SURYO = WK-SURYO  *  -1
**************#2018/06/15 NAV ST
              MOVE WK-BAK-HNDATE TO WRK-DATE2
**************#2018/06/15 NAV ED
              PERFORM  ZAIKO-SEC
**************在庫プラス
              MOVE     HNSU     TO  WK-SURYO
              MOVE     SOKCD    TO  WK-KEY-SOKCD
              MOVE     SKTCD    TO  WK-KEY-SYOCD
              MOVE     HINT1    TO  WK-KEY-HINCD(1:5)
              MOVE     HINT2    TO  WK-KEY-HINCD(6:2)
              MOVE     HINT3    TO  WK-KEY-HINCD(8:1)
              MOVE     TANBAN   TO  WK-KEY-TANBAN
              MOVE     HNSU     TO  WK-SURYO
**************#2018/06/15 NAV ST
              MOVE     HNDATE(1:6) TO WRK-DATE2
**************#2018/06/15 NAV ED
              PERFORM  ZAIKO-SEC
***********END-IF
**************#2017/04/14 NAV ED
              MOVE     MEI-REC    TO  DATA-TAIHI
              REWRITE  MEI-REC
              END-REWRITE
         WHEN     3
*             PERFORM  SFRMEIL1-DEL-SEC
**************#2017/04/14 NAV ST
**************在庫マイナス
***********IF PARA-SYUBETU = "2"
              MOVE     HNSU      TO  WK-SURYO
              MOVE     SOKCD     TO  WK-KEY-SOKCD
              MOVE     SKTCD     TO  WK-KEY-SYOCD
              MOVE     HINT1     TO  WK-KEY-HINCD(1:5)
              MOVE     HINT2     TO  WK-KEY-HINCD(6:2)
              MOVE     HINT3     TO  WK-KEY-HINCD(8:1)
              MOVE WK-BAK-TANBAN TO  WK-KEY-TANBAN
              MOVE WK-BAK-HNSU   TO  WK-SURYO
**************#2018/06/15 NAV ST
              MOVE     HNDATE(1:6) TO WRK-DATE2
**************#2018/06/15 NAV ED
              COMPUTE WK-SURYO = WK-SURYO  *  -1
              PERFORM  ZAIKO-SEC
***********END-IF
**************#2017/04/14 NAV ED
              MOVE     MEI-REC    TO  DATA-TAIHI
              DELETE   SFRMEIL1
              END-DELETE
     END-EVALUATE.
*
 SFRMEIL1-UPDT-EXIT.
     EXIT.
****************************************************************
*  振替情報ファイル更新項目編集
****************************************************************
 SFRMEIL1-SUM-SEC           SECTION.
*
 SFRMEIL1-SUM-00.
     CLOSE           SFRMEIL1.
     OPEN     INPUT  SFRMEIL1.
 SFRMEIL1-SUM-01.
*完納区分
*  入荷時のみ
     IF       PARA-SYUBETU   NOT =  "2"
              GO             TO        SFRMEIL1-SUM-02
     END-IF.
*
     MOVE     KANKBN         TO        HED-F24.
     IF       KANKBN    =    "1"
         IF   BK-KANKBN   =    " "
              MOVE  SYS-DATE      TO   HED-F25
              MOVE  KANTAN(1:4)   TO   HED-F26
              MOVE  KANTAN(6:2)   TO   HED-F27
         END-IF
     ELSE
              MOVE  ZERO          TO   HED-F25
              MOVE  SPACE         TO   HED-F26
                                       HED-F27
     END-IF.
*
 SFRMEIL1-SUM-02.
*発注数合計／入荷数合計　最終発注日／最終入荷日
     MOVE     SPACE          TO        SUM-HACBUMON
                                       SUM-HACTANTO
                                       SUM-NYKBUMON
                                       SUM-NYKTANTO.
     MOVE     ZERO           TO        SUM-HACGK
                                       SUM-NYKGK
                                       SUM-HACDAT
                                       SUM-NYKDAT.
*
*T↓
*    DISPLAY  "DENKCD="   DENKCD    UPON     CONS.
*    DISPLAY  "NENDO ="   NENDO     UPON     CONS.
*    DISPLAY  "SEASON="   SEASON    UPON     CONS.
*    DISPLAY  "SOKCD ="   SOKCD     UPON     CONS.
*    DISPLAY  "SKTCD ="   SKTCD     UPON     CONS.
*    DISPLAY  "HINT1 ="   HINT1     UPON     CONS.
*    DISPLAY  "HINT2 ="   HINT2     UPON     CONS.
*    DISPLAY  "HINT3 ="   HINT3     UPON     CONS.
*T↑
     MOVE     DENKCD         TO        MEI-F01.
     MOVE     NENDO          TO        MEI-F02.
     MOVE     SEASON         TO        MEI-F03.
     MOVE     SOKCD          TO        MEI-F04.
     MOVE     SKTCD          TO        MEI-F05.
     MOVE     HINT1          TO        MEI-F06.
     MOVE     HINT2          TO        MEI-F07.
     MOVE     HINT3          TO        MEI-F08.
     MOVE     SPACE          TO        MEI-F11.
     MOVE     ZERO           TO        MEI-F12.
     START    SFRMEIL1  KEY  IS  >=    MEI-F01
                                       MEI-F02
                                       MEI-F03
                                       MEI-F04
                                       MEI-F05
                                       MEI-F06
                                       MEI-F07
                                       MEI-F08
        INVALID
              GO             TO        SFRMEIL1-SUM-05
     END-START.
*
 SFRMEIL1-SUM-03.
     READ     SFRMEIL1  NEXT
        AT END
              GO             TO        SFRMEIL1-SUM-05
     END-READ.
*
 SFRMEIL1-SUM-04.
     IF    (  DENKCD   =   MEI-F01  )  AND
           (  NENDO    =   MEI-F02  )  AND
           (  SEASON   =   MEI-F03  )  AND
           (  SOKCD    =   MEI-F04  )  AND
           (  SKTCD    =   MEI-F05  )  AND
           (  HINT1    =   MEI-F06  )  AND
           (  HINT2    =   MEI-F07  )  AND
           (  HINT3    =   MEI-F08  )
              IF ( MEI-F11  =  "1"  )
                   MOVE    MEI-F12     TO    SUM-HACDAT
                   ADD     MEI-F14     TO    SUM-HACGK
*T↓
*                  DISPLAY "BUMON=" MEI-F96 UPON CONS
*                  DISPLAY "TANTO=" MEI-F97 UPON CONS
*T↑
                   MOVE    MEI-F96     TO    SUM-HACBUMON
                   MOVE    MEI-F97     TO    SUM-HACTANTO
              ELSE
                   MOVE    MEI-F12     TO    SUM-NYKDAT
                   ADD     MEI-F14     TO    SUM-NYKGK
                   MOVE    MEI-F96     TO    SUM-NYKBUMON
                   MOVE    MEI-F97     TO    SUM-NYKTANTO
              END-IF
     ELSE
              GO             TO        SFRMEIL1-SUM-05
     END-IF.
*
     GO                      TO        SFRMEIL1-SUM-03.
*
 SFRMEIL1-SUM-05.
     CLOSE           SFRMEIL1.
     OPEN     I-O    SFRMEIL1.
*
 SFRMEIL1-SUM-EXIT.
     EXIT.
****************************************************************
*  振替情報ファイル更新
****************************************************************
 SFRHEDL1-UPDT-SEC      SECTION.
*
     MOVE     SUM-HACGK      TO       HED-F15.
     MOVE     SUM-HACBUMON   TO       HED-F16.
     MOVE     SUM-HACTANTO   TO       HED-F17.
     MOVE     SUM-HACDAT     TO       HED-F18.
     MOVE     SUM-NYKGK      TO       HED-F19.
     MOVE     SUM-NYKBUMON   TO       HED-F20.
     MOVE     SUM-NYKTANTO   TO       HED-F21.
     MOVE     SUM-NYKDAT     TO       HED-F22.
*
     MOVE     PARA-BUMONCD   TO       HED-F96.
     MOVE     PARA-TANCD     TO       HED-F97.
     MOVE     SYS-DATE       TO       HED-F98.
     MOVE     WK-TIME(1:6)   TO       HED-F99.
*
     REWRITE  HED-REC.
*
 SFRHEDL1-UPDT-EXIT.
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
*#2017/05/01 NAV ST 未入庫数更新（発注時）を追加
     EVALUATE PARA-SYUBETU
         WHEN "1"
**********未入庫数
          COMPUTE ZAI-F26  =  ZAI-F26  +  WK-SURYO
         WHEN "2"
**********在庫数更新
          COMPUTE ZAI-F04  =  ZAI-F04  +  WK-SURYO
**********入庫数更新
     DISPLAY "WRK-DATE01 = " WRK-DATE1R1 " : " WRK-DATE2 UPON CONS
          IF      WRK-DATE1R1  <  WRK-DATE2     *>次月
                  COMPUTE ZAI-F11  =  ZAI-F11  +  WK-SURYO
          ELSE                                *>当月
                  COMPUTE ZAI-F06  =  ZAI-F06  +  WK-SURYO
                  COMPUTE ZAI-F07  =  ZAI-F07  +  WK-SURYO
          END-IF
**********未入庫数（減算なので符号を変更する。）
          COMPUTE WK-SURYOZAN = WK-SURYO  *  -1
          COMPUTE ZAI-F26  =  ZAI-F26  +  WK-SURYOZAN
     END-EVALUATE.
*#2017/05/01 NAV ED
*商品名カナセット
     IF   HMEIMS-INV-FLG =  SPACE
          MOVE  MES-F031    TO   ZAI-F30
     ELSE
          MOVE  SPACE       TO   ZAI-F30
     END-IF.
*登録日／更新日セット
     MOVE DATE-AREA         TO   ZAI-F98.
     MOVE DATE-AREA         TO   ZAI-F99.
*
     WRITE  ZAI-REC.
*
 ZAIKO-WRITE-EXIT.
     EXIT.
****************************************************************
*    在庫更新処理２
****************************************************************
 ZAIKO-REWRITE-SEC          SECTION.
*在庫数更新
*****COMPUTE ZAI-F04  =  ZAI-F04  +  WK-SURYO.
*入庫数更新
*****IF      WRK-DATE1  <  WRK-DATE2     *>次月
*****        COMPUTE ZAI-F11  =  ZAI-F11  +  WK-SURYO
*****ELSE                                *>当月
*****        COMPUTE ZAI-F06  =  ZAI-F06  +  WK-SURYO
*****        COMPUTE ZAI-F07  =  ZAI-F07  +  WK-SURYO
*****END-IF.
*#2017/05/01 NAV ST 未入庫数更新（発注時）を追加
     EVALUATE PARA-SYUBETU
         WHEN "1"
**********未入庫数
          COMPUTE ZAI-F26  =  ZAI-F26  +  WK-SURYO
         WHEN "2"
**********在庫数更新
          COMPUTE ZAI-F04  =  ZAI-F04  +  WK-SURYO
**********入庫数更新
     DISPLAY "WRK-DATE02 = " WRK-DATE1R1 " : " WRK-DATE2 UPON CONS
          IF      WRK-DATE1R1  <  WRK-DATE2     *>次月
                  COMPUTE ZAI-F11  =  ZAI-F11  +  WK-SURYO
          ELSE                                *>当月
                  COMPUTE ZAI-F06  =  ZAI-F06  +  WK-SURYO
                  COMPUTE ZAI-F07  =  ZAI-F07  +  WK-SURYO
          END-IF
**********未入庫数（減算なので符号を変更する。）
          COMPUTE WK-SURYOZAN = WK-SURYO  *  -1
          COMPUTE ZAI-F26  =  ZAI-F26  +  WK-SURYOZAN
     END-EVALUATE.
*#2017/05/01 NAV ED
*更新日セット
     MOVE DATE-AREA         TO   ZAI-F99.
*
     REWRITE  ZAI-REC.
*
 ZAIKO-REWRITE-EXIT.
     EXIT.
